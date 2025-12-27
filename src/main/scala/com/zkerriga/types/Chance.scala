package com.zkerriga.types

opaque type Chance = BigDecimal

object Chance:
  val NoChance: Chance = 0
  val CoinFlip: Chance = 0.5
  val Certain: Chance = 1

  extension (chance: Chance)
    infix def or(other: Chance): Chance = chance + other
    infix def and(other: Chance): Chance = chance * other
    infix def in(other: Chance): Chance = chance / other
