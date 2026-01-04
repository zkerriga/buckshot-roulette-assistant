package com.zkerriga.buckshot.engine.ai

import cats.data.NonEmptySeq
import cats.kernel.Eq
import cats.syntax.all.*
import com.zkerriga.buckshot.engine.Distribution
import com.zkerriga.buckshot.engine.state.PrivateStates.DealerNotes
import com.zkerriga.buckshot.engine.state.Revealed
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state.items.Slot
import com.zkerriga.buckshot.game.state.partitipant.Items.ItemOn
import com.zkerriga.types.Nat

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.*

object DealerAi:
  enum Action:
    case Use(item: ItemOn)
    case Steal(item: ItemOn)
    case Shoot(target: Side)
  object Action:
    given Eq[Action] = Eq.fromUniversalEquals

  enum NextShell:
    case Unknown
    case Known(shell: Shell)

  object NextShell:
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

  import NextShell.*

  def desiredItems(table: TableState, alreadyUsedMeds: Boolean, next: NextShell): Set[RegularItem] = {
    import table.*
    val canUseCigs = dealer.has(Cigarettes) || (dealer.hasAdrenaline && player.has(Cigarettes))
    RegularItem.All.filter {
      case MagnifyingGlass => next == Unknown && shotgun.total > Nat[1]
      case Cigarettes => dealer.health < maxHealth.maxAllowed
      case Meds => dealer.health < maxHealth.maxAllowed && !alreadyUsedMeds && !canUseCigs
      case Beer => next != Known(Live) && shotgun.total > Nat[1]
      case Handcuffs => player.hands.free && shotgun.total > Nat[1]
      case Saw => shotgun.damage != Damage.Double && next == Known(Live)
      case BurnerPhone => shotgun.total > Nat[2]
      case Inverter => next == Known(Blank)
    }
  }

  @tailrec
  private def search(
    slotsGroups: List[Set[Slot]],
    dealerItems: Items,
    wanted: RegularItem => Boolean,
  ): Option[NonEmptySeq[ItemOn]] =
    slotsGroups match
      case group :: nextGroups =>
        NonEmptySeq.fromSeq(dealerItems.positioned.filter(itemOn => group(itemOn.on) && wanted(itemOn.item))) match
          case Some(found) => Some(found)
          case None => search(nextGroups, dealerItems, wanted)
      case Nil => None

  /** We don't know the exact order of Dealer items, but we know in which order Dealer received groups of them. We also
    * know which items Dealer wants to use on the current turn.
    *
    * That means as soon as we find the first group of items that contains at least one desired item, we can guarantee,
    * that dealer is NOT going to check any other items.
    *
    * If group is found, we assume that the probability of every desired item to be used first is equal to the number of
    * items of that type in the group.
    */
  def usesDealerItem(
    dealerItems: Items,
    slotGroups: List[Set[Slot]],
    desired: Set[RegularItem],
  ): Option[Distribution[Action.Use]] =
    search(slotGroups, dealerItems, desired.contains).map { desiredItems =>
      Distribution.weighted(desiredItems.map[(Nat, Action.Use)](item => Nat[1] -> Action.Use(item)))
    }

  /** Since the order of Player items is known and Dealer always steals the first desired item they find, we can just
    * deterministically pick the first one
    */
  def stealsPlayerItem(dealerItems: Items, playerItems: Items, desired: Set[RegularItem]): Option[Action.Steal] =
    if dealerItems.containAdrenaline then
      playerItems.positioned
        .find(itemOn => desired.contains(itemOn.item))
        .map(Action.Steal(_))
    else None

  private enum SimpleChance:
    case FiftyFifty
    case Rather(shell: Shell)

  private object SimpleChance:
    /** @note
      *   dealer does NOT consider their knowledge about revealed shells when evaluating [[Live]]/[[Blank]] chances
      */
    def evaluate(shotgun: Shotgun): SimpleChance =
      if shotgun.live == shotgun.blank then FiftyFifty
      else if shotgun.live > shotgun.blank then Rather(Live)
      else Rather(Blank)

  import SimpleChance.*

  def preparesToShoot(table: TableState, slotGroups: List[Set[Slot]], next: NextShell): Distribution[Action] =
    import table.*

    /** dealer has already made their decision to shoot Player with a Saw
      */
    if shotgun.damage == Damage.Double then Distribution.deterministic(Action.Shoot(Player))
    else {
      val shellExpectation =
        next match
          case Known(shell) => Distribution.deterministic(shell)
          case Unknown =>
            SimpleChance.evaluate(shotgun) match
              case FiftyFifty => Distribution.weighted(Nat[1] -> Live, Nat[1] -> Blank)
              case Rather(shell) => Distribution.deterministic(shell)

      shellExpectation.flatMap {
        case Live =>
          usesDealerItem(dealer.items, slotGroups, Set(Saw)).getOrElse {
            Distribution.deterministic {
              stealsPlayerItem(dealer.items, player.items, Set(Saw)).getOrElse(Action.Shoot(Player))
            }
          }
        case Blank => Distribution.deterministic(Action.Shoot(Dealer))
      }
    }

  def next(table: TableState, notes: DealerNotes, revealed: Revealed): Distribution[Action] =
    import table.*

    val nextShell = NextShell.figureOut(shotgun, revealed)
    val desired = desiredItems(table, notes.usedMeds, nextShell)

    usesDealerItem(dealer.items, notes.slotGroups, desired)
      .orElse(stealsPlayerItem(dealer.items, player.items, desired).map(Distribution.deterministic))
      .getOrElse(preparesToShoot(table, notes.slotGroups, nextShell))
