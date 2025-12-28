package com.zkerriga.types

import spire.math.Rational

import java.math.RoundingMode

opaque type Chance = Rational

object Chance:
  val NoChance: Chance = 0
  val CoinFlip: Chance = Rational(1, 2)
  val Certain: Chance = 1

  extension (chance: Chance)
    infix def or(other: Chance): Chance = chance + other
    infix def and(other: Chance): Chance = chance * other
    infix def in(other: Chance): Chance = chance / other
    def show: String = s"${chance.toBigDecimal(6, RoundingMode.HALF_UP) * 100}%"

  def binary(value: Boolean): Chance = if value then Certain else NoChance

  given Ordering[Chance] = summon[Ordering[Rational]]
