package com.zkerriga.buckshot.engine

import cats.Eq
import com.zkerriga.buckshot.engine.EngineError.V
import com.zkerriga.types.Chance

opaque type BeliefState[+A] = Distribution[A]

object BeliefState:
  def deterministic[A](value: A): BeliefState[A] =
    Distribution.deterministic(value)

  /** must be constructed with positive chances that sum up to 100%
    */
  def unsafe[A](value: (Chance, A), values: (Chance, A)*): BeliefState[A] =
    Distribution.unsafe(value, values*)

  extension [A](belief: BeliefState[A])
    def getDistribution: Distribution[A] = belief
    def asSortedSeq(using Ordering[Chance]): Seq[(chance: Chance, value: A)] =
      belief.asSeq.sortBy(_.chance)

    /** @param howPossible
      *   is a function that given a value of type [[A]] returns how possible is this value based on some new
      *   observation
      */
    def conditioning(howPossible: A => Chance): V[BeliefState[A]] =
      belief
        .updateChances: (chance, value) =>
          chance and howPossible(value)
        .toRight(EngineError.BadConditioning)

    /** transform deterministically without changing the probabilities
      */
    def update(f: A => A)(using Eq[A]): BeliefState[A] =
      belief.map(f).deduplicate

    /** @note
      *   currently assumes that all [[A]] are unique and their transformations also result in unique values, so no
      *   deduplication is required, todo: verify
      */
    def transform(transformation: A => Distribution[A]): BeliefState[A] =
      belief.flatMap(transformation)
