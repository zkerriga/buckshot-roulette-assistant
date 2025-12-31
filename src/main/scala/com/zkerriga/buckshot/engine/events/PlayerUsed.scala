package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.engine.BeliefState
import com.zkerriga.buckshot.engine.DealerBeliefChecks.{missOnGlassReveal, missOnPhoneReveal, missOnShellOut}
import com.zkerriga.buckshot.engine.EngineError.*
import com.zkerriga.buckshot.engine.events.PlayerUsed.ItemUse
import com.zkerriga.buckshot.engine.state.{GameState, Knowledge, Revealed}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Used
import com.zkerriga.buckshot.game.events.outcome.Outcome.{GameOver, Reset}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.partitipant.Side.Player
import com.zkerriga.types.Chance

case class PlayerUsed(item: ItemUse, stolen: Boolean)

object PlayerUsed:
  enum ItemUse:
    case Handcuffs
    case MagnifyingGlass(revealed: Shell)
    case Beer(out: Shell)
    case Cigarettes
    case Saw
    case Inverter
    case BurnerPhone(revealed: Shell, at: SeqNr)
    case Meds(good: Boolean)

  def execute(state: GameState, used: PlayerUsed): V[GameOver | Reset | GameState] =
    Used
      .execute(state.public, Used(Player, ItemUse.asPublic(used.item), used.stolen))
      .flatMap:
        case outcome: (GameOver | Reset) => outcome.ok
        case table: TableState =>
          for dealerKnowledge <- updateDealerKnowledge(state, table, used.item)(state.knowledge.dealer)
          yield GameState(
            public = table,
            knowledge = Knowledge(
              dealer = dealerKnowledge,
              player = updatePlayerKnowledge(state.knowledge.player, used.item),
            ),
          )

  private def updatePlayerKnowledge(knowledge: Revealed, item: ItemUse): Revealed =
    item match
      case ItemUse.MagnifyingGlass(revealed) => knowledge.revealed(revealed, Shell1)
      case ItemUse.BurnerPhone(revealed, at) => knowledge.revealed(revealed, at)
      case ItemUse.Beer(out) => knowledge.afterShellOut
      case _ => knowledge

  private def updateDealerKnowledge(
    oldState: GameState,
    table: TableState,
    item: ItemUse,
  )(belief: BeliefState[Revealed]): V[BeliefState[Revealed]] =
    item match
      case ItemUse.MagnifyingGlass(revealed) =>
        belief.conditioning: knowledge =>
          Chance.certainUnless(missOnGlassReveal(knowledge, table.shotgun, revealed))

      case ItemUse.BurnerPhone(revealed, at) =>
        belief.conditioning: knowledge =>
          Chance.certainUnless(missOnPhoneReveal(knowledge, table.shotgun, revealed, at))

      case ItemUse.Beer(out) =>
        for adjusted <- belief.conditioning: knowledge =>
            Chance.certainUnless(missOnShellOut(knowledge, old = oldState.shotgun, updated = table.shotgun, out = out))
        yield adjusted.update(_.afterShellOut)

      case _ => belief.ok

  object ItemUse:
    private[PlayerUsed] val asPublic: ItemUse => Used.ItemUse =
      case ItemUse.Handcuffs => Used.ItemUse.Handcuffs
      case ItemUse.MagnifyingGlass(revealed) => Used.ItemUse.MagnifyingGlass
      case ItemUse.Beer(out) => Used.ItemUse.Beer(out)
      case ItemUse.Cigarettes => Used.ItemUse.Cigarettes
      case ItemUse.Saw => Used.ItemUse.Saw
      case ItemUse.Inverter => Used.ItemUse.Inverter
      case ItemUse.BurnerPhone(revealed, at) => Used.ItemUse.BurnerPhone
      case ItemUse.Meds(good) => Used.ItemUse.Meds(good)
