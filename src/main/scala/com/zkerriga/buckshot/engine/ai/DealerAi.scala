package com.zkerriga.buckshot.engine.ai

import cats.syntax.all.*
import com.zkerriga.buckshot.engine.state.Revealed
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.types.Nat
import scala.math.Ordering.Implicits.*

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

    def wantsMagnifyingGlass = nextShell == Unknown
    def wantsHandcuffs = player.hands.free && shotgun.total > Nat[1]
    def wantsCigarettes = dealer.health < maxHealth.maxAllowed
    def wantsMeds = dealer.health < maxHealth.maxAllowed && dealer.health > Health[1] && !canUse(Cigarettes)
    def wantsBeer = nextShell != Known(Live) && shotgun.total > Nat[1]
    def wantsSaw = shotgun.damage != Damage.Double && nextShell == Known(Live)
    def wantsBurnerPhone = shotgun.total > Nat[2]
    def wantsInverter = nextShell == Known(Blank)

    def wants(item: RegularItem, condition: Boolean): Option[RegularItem] =
      Option.when(canUse(item) && condition)(item)

    val wantsToUse =
      wants(MagnifyingGlass, wantsMagnifyingGlass)
        .orElse(wants(Handcuffs, wantsHandcuffs))
        .orElse(wants(Cigarettes, wantsCigarettes))
        .orElse(wants(Meds, wantsMeds))
        .orElse(wants(Beer, wantsBeer))
        .orElse(wants(Saw, wantsSaw))
        .orElse(wants(BurnerPhone, wantsBurnerPhone))
        .orElse(wants(Inverter, wantsInverter))

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
