package com.zkerriga.types

/** A positive natural number
  */
opaque type Quantity = Nat

object Quantity:
  val One: Quantity = Nat.One
  val Two: Quantity = Nat.Two
  val Three: Quantity = Nat.Three
  val Four: Quantity = Nat.Four

  import Nat.increase as inc
  import Nat.decrease as dec

  extension (quantity: Quantity)
    def increase: Quantity = quantity.inc
    def decrease: Option[Quantity] = Option.unless(quantity == One)(quantity.dec).flatten
