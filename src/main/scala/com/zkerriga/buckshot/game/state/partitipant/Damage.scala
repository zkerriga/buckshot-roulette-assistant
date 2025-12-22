package com.zkerriga.buckshot.game.state.partitipant

enum Damage:
  case Single, Double

object Damage:
  extension (damage: Damage)
    def increased: Option[Damage] =
      damage match
        case Single => Some(Double)
        case Double => None
