package com.zkerriga.types

/** A positive natural number
  */
opaque type Quantity = Nat

object Quantity:
  import Nat.{decrease as dec, increase as inc}

  extension (quantity: Quantity)
    def increase: Quantity = quantity.inc
    def decrease: Option[Quantity] = Option.unless(quantity == Nat[1])(quantity.dec).flatten

  inline def apply[N <: Int](using ev: ValueOf[N]): Quantity =
    inline if ev.value > 0 then Nat[N]
    else scala.compiletime.error("Quantity must be a positive natural number")
