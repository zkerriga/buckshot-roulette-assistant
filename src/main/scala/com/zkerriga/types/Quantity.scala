package com.zkerriga.types

/** A positive natural number
  */
opaque type Quantity <: Nat = Nat

object Quantity:
  extension (quantity: Quantity)
    def increased: Quantity = quantity plus Nat[1]
    def decreased: Option[Quantity] = Option.unless(quantity == Nat[1])(quantity minus Nat[1]).flatten

  inline def apply[N <: Int](using ev: ValueOf[N]): Quantity =
    inline if ev.value > 0 then Nat[N]
    else scala.compiletime.error("Quantity must be a positive natural number")
