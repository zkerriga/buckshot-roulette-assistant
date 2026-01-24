package com.zkerriga.buckshot.game.state.shotgun

import com.zkerriga.buckshot.game.events.outcome.StateError.*
import com.zkerriga.buckshot.game.state.partitipant.Damage
import com.zkerriga.buckshot.game.state.shotgun.Shell.*
import com.zkerriga.types.Nat
import com.zkerriga.types.steps.ResultExtension.*
import steps.result.Result

case class Shotgun(
  shells: Shotgun.ShellDistribution,
  effects: Shotgun.Effects,
):
  export shells.*
  export effects.*

object Shotgun:
  case class ShellDistribution(live: Nat, blank: Nat):
    val total: Nat = live plus blank

  case class Effects(
    damage: Damage,
    inverted: Boolean,
  )
  object Effects:
    val Default: Effects = Effects(
      damage = Damage.Single,
      inverted = false,
    )

  def fresh(live: Nat, blank: Nat): Shotgun =
    Shotgun(
      shells = ShellDistribution(
        live = live,
        blank = blank,
      ),
      effects = Effects.Default,
    )

  extension (shotgun: Shotgun)
    /** Covers both shot and beer scenarios, but I'm not actually sure what happens with non-default [[Effects]] after
      * using beer. It would be logical to keep them, todo: requires checking
      */
    def shellOut(shell: Shell)(using Raise[ShotgunStateMismatch.type]): Option[Shotgun] =
      val actual = shell.considering(shotgun.effects)
      val shellsOpt = actual match
        case Live => (shotgun.live minus Nat[1]).map(updated => shotgun.shells.copy(live = updated))
        case Blank => (shotgun.blank minus Nat[1]).map(updated => shotgun.shells.copy(blank = updated))
      val shells = shellsOpt.getOrRaise[ShotgunStateMismatch.type](ShotgunStateMismatch)
      Option.unless(shells.total == Nat[0]):
        Shotgun(
          shells = shells,
          effects = Effects.Default,
        )

    def sawApplied(using Raise[SawAlreadyUsed.type]): Shotgun =
      val updated = shotgun.damage.increased.getOrRaise[SawAlreadyUsed.type](SawAlreadyUsed)
      shotgun.copy(effects = shotgun.effects.copy(damage = updated))

    def inverterApplied: Shotgun =
      shotgun.copy(effects = shotgun.effects.copy(inverted = !shotgun.effects.inverted))
