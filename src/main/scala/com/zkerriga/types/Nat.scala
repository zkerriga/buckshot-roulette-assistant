package com.zkerriga.types

import scala.collection.IterableOps

/** A natural number
  */
opaque type Nat <: Int = Int

object Nat:
  extension (number: Nat)
    infix def plus(other: Nat): Nat = number + other
    infix def minus(other: Nat): Option[Nat] = Option.unless(number < other)(number - other)

  inline def apply[N <: Int](using ev: ValueOf[N]): Nat =
    inline if ev.value >= 0 then ev.value
    else scala.compiletime.error("Nat must be a natural number")

  extension [A, CC[_], C <: IterableOps[A, CC, C]](iterable: C)
    def countNat(f: A => Boolean): Nat = iterable.iterator.count(f)
