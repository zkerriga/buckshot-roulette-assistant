package com.zkerriga.buckshot.engine.ai

import cats.syntax.all.*
import com.zkerriga.buckshot.engine.state.Revealed
import com.zkerriga.buckshot.game.events.{Shot, Used}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.types.Nat

object DealerAi {
  type NextAction = Shot[Dealer.type] | Used[Dealer.type]

  def next(state: TableState, knowledge: Revealed): NextAction = {
    import state.*

    val nextShell = figureOutNextShell(shotgun, knowledge)

    def canUse(item: RegularItem): Boolean =
      (dealer has item) || ((dealer has Adrenaline) && (player has item))

    val wantsToUse =
      if canUse(MagnifyingGlass) && nextShell == NextShell.Unknown then MagnifyingGlass.some
      else if canUse(Handcuffs) && player.hands.free && shotgun.total > Nat[1] then Handcuffs.some
      else if canUse(Cigarettes) && dealer.health != maxHealth.max then Cigarettes.some
      else if canUse(Meds) && dealer.health != maxHealth.max && dealer.health > Health[1] && !canUse(Cigarettes) then
        Meds.some
      else if canUse(Beer) && nextShell != NextShell.Known(Live) && shotgun.total > Nat[1] then Beer.some
      else if canUse(Saw) && shotgun.damage != Damage.Double && nextShell == NextShell.Known(Live) then Saw.some
      else if canUse(BurnerPhone) && shotgun.total > Nat[2] then BurnerPhone.some
      else if canUse(Inverter) && nextShell == NextShell.Known(Blank) then Inverter.some
      else None

    // todo: correct the following logic considering it was implemented with action batches
    ???
  }

  private def figureOutNextShell(shotgun: Shotgun, revealed: Revealed): NextShell =
    revealed
      .get(Shell1)
      .orElse:
        if shotgun.live == revealed.count(Live) then Blank.some
        else if shotgun.blank == revealed.count(Blank) then Live.some
        else None
      .map: shell =>
        NextShell.Known(if shotgun.inverted then shell.inverted else shell)
      .getOrElse(NextShell.Unknown)

  private enum NextShell:
    case Unknown
    case Known(shell: Shell)
}
