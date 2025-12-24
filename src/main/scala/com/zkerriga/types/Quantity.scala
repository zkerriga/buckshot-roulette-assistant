package com.zkerriga.types

/** A positive natural number
  */
opaque type Quantity = Nat

object Quantity:
  import Nat.{decreased as dec, increased as inc}

  extension (quantity: Quantity)
    def increased: Quantity = quantity.inc
    def decreased: Option[Quantity] = Option.unless(quantity == Nat[1])(quantity.dec).flatten

  given Ordering[Quantity] = summon[Ordering[Nat]]
  given Conversion[Quantity, Nat] = identity
  given Conversion[Quantity, Int] = summon[Conversion[Nat, Int]].apply

  inline def apply[N <: Int](using ev: ValueOf[N]): Quantity =
    inline if ev.value > 0 then Nat[N]
    else scala.compiletime.error("Quantity must be a positive natural number")
