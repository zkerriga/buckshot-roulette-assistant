package com.zkerriga.types

/** A natural number
  */
opaque type Nat = Int

object Nat:
  val Zero: Nat = 0
  val One: Nat = 1
  val Two: Nat = 2
  val Three: Nat = 3
  val Four: Nat = 4

  extension (number: Nat)
    def increase: Nat = number + 1
    def decrease: Option[Nat] = Option.unless(number == Zero)(number - 1)
    def +(other: Nat): Nat = number + other
