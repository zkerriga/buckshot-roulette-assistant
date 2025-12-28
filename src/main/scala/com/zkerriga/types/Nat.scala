package com.zkerriga.types

/** A natural number
  */
opaque type Nat = Int

object Nat:
  extension (number: Nat)
    def increased: Nat = number + 1
    def decreased: Option[Nat] = Option.unless(number == 0)(number - 1)
    def +(other: Nat): Nat = number + other

  given Ordering[Nat] = Ordering.Int
  given Conversion[Nat, Int] = identity

  inline def apply[N <: Int](using ev: ValueOf[N]): Nat =
    inline if ev.value >= 0 then ev.value
    else scala.compiletime.error("Nat must be a natural number")

  extension [A](iterable: Iterable[A]) def countNat(f: A => Boolean): Nat = iterable.iterator.count(f)
