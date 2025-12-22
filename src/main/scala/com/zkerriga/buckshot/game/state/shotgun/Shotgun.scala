package com.zkerriga.buckshot.game.state.shotgun

import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.*
import com.zkerriga.buckshot.game.state.partitipant.Damage
import com.zkerriga.buckshot.game.state.shotgun.Shell.*
import com.zkerriga.types.Nat

case class Shotgun(
  shells: Shotgun.ShellDistribution,
  effects: Shotgun.Effects,
):
  export shells.*
  export effects.*

object Shotgun:
  case class ShellDistribution(live: Nat, blank: Nat):
    val total: Nat = live + blank

  case class Effects(
    damage: Damage,
    inverted: Boolean,
  )
  object Effects:
    val Default: Effects = Effects(
      damage = Damage.Single,
      inverted = false,
    )

  extension (shotgun: Shotgun)
    /** Covers both shot and beer scenarios, but I'm not actually sure what happens with non-default [[Effects]] after
      * using beer. It would be logical to keep them, todo: requires checking
      */
    def shellOut(shell: Shell): V[Option[Shotgun]] =
      val actual = if shotgun.inverted then shell.inverted else shell
      val shells = actual match
        case Live => shotgun.live.decreased.map(updated => shotgun.shells.copy(live = updated))
        case Blank => shotgun.blank.decreased.map(updated => shotgun.shells.copy(blank = updated))
      shells
        .toRight(ShotgunStateMismatch)
        .map: shells =>
          Option.unless(shells.total == Nat[0]):
            Shotgun(
              shells = shells,
              effects = Effects.Default,
            )

    def sawApplied: V[Shotgun] =
      shotgun.damage.increased
        .toRight(SawAlreadyUsed)
        .map: updated =>
          shotgun.copy(effects = shotgun.effects.copy(damage = updated))

    def inverterApplied: Shotgun =
      shotgun.copy(effects = shotgun.effects.copy(inverted = !shotgun.effects.inverted))
