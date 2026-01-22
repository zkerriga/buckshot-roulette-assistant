package com.zkerriga.buckshot.engine

import algebra.Semigroup
import cats.Eq
import cats.syntax.all.*
import cats.data.NonEmptySeq
import com.zkerriga.types.{Chance, Nat}

/** Probabilistic distribution of possible [[A]] values
  *
  * The sum of [[Distribution.Possible.chance]] is always equal to 100%
  */
opaque type Distribution[+A] = NonEmptySeq[Distribution.Possible[A]]

object Distribution:
  /** [[A]] can be called [[Possible]] when its chance is positive
    */
  type Possible[+A] = (chance: Chance, value: A)

  def deterministic[A](value: A): Distribution[A] =
    unsafe(Chance.Certain -> value)

  def weighted[A](values: NonEmptySeq[(weight: Nat, value: A)]): Distribution[A] =
    val total = values.map(_.weight).reduce(using _ plus _)
    values.map: (weight, value) =>
      Chance.wighted(weight, total) -> value

  def weighted[A](value: (weight: Nat, value: A), values: (weight: Nat, value: A)*): Distribution[A] =
    weighted(NonEmptySeq(value, values))

  extension [A](values: Distribution[A])
    def asSeq: Seq[Possible[A]] = values.toSeq
    def sumMerged[B](merge: Possible[A] => B)(using Semigroup[B]): B =
      values.map(merge).reduce

    /** @note
      *   call on deduplicated [[Distribution]]
      */
    def chanceOf(value: A)(using Eq[A]): Chance =
      values
        .find(_.value === value)
        .map(_.chance)
        .getOrElse(Chance.NoChance)

    def exists(f: A => Boolean): Boolean =
      values.exists: (_, value) =>
        f(value)

    def map[B](f: A => B): Distribution[B] =
      values.map: (chance, value) =>
        chance -> f(value)

    def flatMap[B](f: A => Distribution[B]): Distribution[B] =
      values.flatMap: (aChance, a) =>
        f(a).map: (bChance, b) =>
          (aChance and bChance) -> b

    def foldLeft[B](initial: B)(f: (B, Possible[A]) => B): B =
      values.foldLeft(initial)(f)

    def updateChances(f: (Chance, A) => Chance): Option[Distribution[A]] =
      NonEmptySeq
        .fromSeq:
          values.toSeq.flatMap: (chance, value) =>
            val updated = f(chance, value)
            Option.unless(updated == Chance.NoChance)(updated -> value)
        .map(normalize)

    def deduplicate(using Eq[A]): Distribution[A] =
      val (head, rest) = values.tail.foldLeft((head = values.head, rest = Map.empty[A, Chance])): (acc, possible) =>
        if possible.value === acc.head.value then ((acc.head.chance or possible.chance) -> acc.head.value, acc.rest)
        else
          acc.head -> acc.rest.updatedWith(possible.value):
            case Some(existing) => Some(existing or possible.chance)
            case None => Some(possible.chance)
      NonEmptySeq(head, rest.toSeq.map(_.swap))

  /** must be constructed with positive chances that sum up to 100%
    */
  def unsafe[A](value: Possible[A], values: Possible[A]*): Distribution[A] =
    require((value +: values).forall(_.chance != Chance.NoChance)) // todo: for testing
    require(values.foldLeft(value.chance)(_ or _.chance) == Chance.Certain) // todo: for testing
    NonEmptySeq(value, values)

  private def normalize[A](values: NonEmptySeq[Possible[A]]): NonEmptySeq[Possible[A]] =
    val totalChance = values.map(_.chance).reduce(using _ or _)
    values.map: (chance, value) =>
      (chance in totalChance) -> value
