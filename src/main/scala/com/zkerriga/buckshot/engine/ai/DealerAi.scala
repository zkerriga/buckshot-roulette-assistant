package com.zkerriga.buckshot.engine.ai

import cats.syntax.all.*
import com.zkerriga.buckshot.engine.state.Revealed
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.types.Nat

object DealerAi:
  enum Action:
    case Use[Item <: RegularItem, Steal <: Boolean](item: Item, steal: Steal)
    case Shoot[Target <: Side](target: Target)
    case Guess(live: Use[Saw.type, false] | Shoot[Player.type], blank: Shoot[Dealer.type])

  def next(state: TableState, knowledge: Revealed): Action =
    import state.*
    import NextShell.*
    import SimpleChance.*

    val nextShell = figureOut(shotgun, knowledge)

    def canUse(item: RegularItem): Boolean =
      (dealer has item) || ((dealer has Adrenaline) && (player has item))

    val wantsToUse =
      if canUse(MagnifyingGlass) && nextShell == Unknown then MagnifyingGlass.some
      else if canUse(Handcuffs) && player.hands.free && shotgun.total > Nat[1] then Handcuffs.some
      else if canUse(Cigarettes) && dealer.health != maxHealth.max then Cigarettes.some
      else if canUse(Meds) && dealer.health != maxHealth.max && dealer.health > Health[1] && !canUse(Cigarettes) then
        Meds.some
      else if canUse(Beer) && nextShell != Known(Live) && shotgun.total > Nat[1] then Beer.some
      else if canUse(Saw) && shotgun.damage != Damage.Double && nextShell == Known(Live) then Saw.some
      else if canUse(BurnerPhone) && shotgun.total > Nat[2] then BurnerPhone.some
      else if canUse(Inverter) && nextShell == Known(Blank) then Inverter.some
      else None

    wantsToUse match
      case Some(item) => Action.Use(item, steal = dealer hasNo item)
      case None =>
        if shotgun.damage != Damage.Double && (dealer has Saw) && nextShell != Known(Blank) then
          SimpleChance.evaluate(shotgun) match
            case FiftyFifty => Action.Guess(live = Action.Use(Saw, steal = false), blank = Action.Shoot(Dealer))
            case Rather(Live) => Action.Use(Saw, steal = false)
            case Rather(Blank) => Action.Shoot(Dealer)
        else if shotgun.damage == Damage.Double then Action.Shoot(Player)
        else
          nextShell match
            case Known(Live) => Action.Shoot(Player)
            case Known(Blank) => Action.Shoot(Dealer)
            case Unknown =>
              SimpleChance.evaluate(shotgun) match
                case FiftyFifty => Action.Guess(live = Action.Shoot(Player), blank = Action.Shoot(Dealer))
                case Rather(Live) => Action.Shoot(Player)
                case Rather(Blank) => Action.Shoot(Dealer)

  private enum NextShell:
    case Unknown
    case Known(shell: Shell)

  private object NextShell:
    def figureOut(shotgun: Shotgun, revealed: Revealed): NextShell =
      revealed
        .get(Shell1)
        .orElse:
          if shotgun.live == revealed.count(Live) then Blank.some
          else if shotgun.blank == revealed.count(Blank) then Live.some
          else None
        .map: shell =>
          Known(shell.considering(shotgun.effects))
        .getOrElse(Unknown)

  private enum SimpleChance:
    case FiftyFifty
    case Rather(shell: Shell)

  private object SimpleChance:
    // todo: does this have to consider dealer's knowledge?
    def evaluate(shotgun: Shotgun): SimpleChance =
      if shotgun.live == shotgun.blank then FiftyFifty
      else if shotgun.live > shotgun.blank then Rather(Live)
      else Rather(Blank)
