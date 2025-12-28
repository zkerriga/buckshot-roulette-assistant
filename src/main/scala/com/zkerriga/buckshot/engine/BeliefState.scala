package com.zkerriga.buckshot.engine

import cats.Eq
import cats.syntax.all.*
import cats.data.NonEmptySeq
import com.zkerriga.buckshot.engine.BeliefState.{Possible, deduplicate, renormalize}
import com.zkerriga.types.Chance

case class BeliefState[A](states: NonEmptySeq[Possible[A]]):
  require(states.forall(_.chance != Chance.NoChance)) // todo: testing
  require(states.map(_.chance).reduce(using _ or _) == Chance.Certain) // todo: testing

  /** @param howPossible
    *   is a function that given a value of type A returns how possible is this value based on some new observation
    */
  def conditioning(howPossible: A => Chance): BeliefState[A] =
    val updated: Option[NonEmptySeq[Possible[A]]] = NonEmptySeq.fromSeq:
      states.toSeq.flatMap: possible =>
        possible.reconsidered(_ and howPossible(possible.value))
    updated match
      case Some(states) => BeliefState(renormalize(states))
      case None =>
        // todo: figure out proper error handling
        throw RuntimeException("conditioning led to an impossible belief state")

  def transform(transformation: A => BeliefState[A])(using Eq[A]): BeliefState[A] =
    val updated = states.flatMap: possible =>
      val outcomes = transformation(possible.value)
      outcomes.states.map(_.adjusted(_ and possible.chance))
    BeliefState(deduplicate(updated))

object BeliefState:
  case class Possible[A](value: A, chance: Chance):
    require(chance != Chance.NoChance) // todo: testing
    def adjusted(f: Chance => Chance): Possible[A] = copy(chance = f(chance))
    def reconsidered(f: Chance => Chance): Option[Possible[A]] =
      val updated = f(chance)
      Option.unless(updated == Chance.NoChance)(copy(chance = updated))

  extension (chance: Chance) infix def ->[A](value: A): Possible[A] = Possible(value, chance)

  def fromUnique[A](value: Possible[A], values: Possible[A]*): BeliefState[A] =
    BeliefState(NonEmptySeq(value, values))

  def deterministic[A](value: A): BeliefState[A] =
    fromUnique:
      Chance.Certain -> value

  private def deduplicate[A: Eq](states: NonEmptySeq[Possible[A]]): NonEmptySeq[Possible[A]] =
    val (head, rest) = states.tail.foldLeft((head = states.head, rest = Map.empty[A, Chance])): (acc, a) =>
      if a.value === acc.head.value then acc.head.adjusted(_ or a.chance) -> acc.rest
      else
        acc.head -> acc.rest.updatedWith(a.value):
          case Some(existing) => Some(existing or a.chance)
          case None => Some(a.chance)
    NonEmptySeq(head, rest.toSeq.map(Possible.apply))

  private def renormalize[A](states: NonEmptySeq[Possible[A]]): NonEmptySeq[Possible[A]] =
    val totalChance = states.map(_.chance).reduce(using _ or _)
    states.map(_.adjusted(_ in totalChance))
