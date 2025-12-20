package com.zkerriga.buckshot.game.state.shotgun

import com.zkerriga.types.Nat

case class Shotgun(
  shells: Shotgun.ShellDistribution,
  effects: Shotgun.Effects,
)

object Shotgun:
  case class ShellDistribution(
    live: Nat,
    blank: Nat,
  )
  case class Effects(
    sawedOff: Boolean,
    inverted: Boolean,
  )
