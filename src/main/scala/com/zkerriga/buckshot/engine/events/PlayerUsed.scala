package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.engine.DealerBeliefChecks.{missOnGlassReveal, missOnPhoneReveal, missOnShellOut}
import com.zkerriga.buckshot.engine.EngineError.*
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerKnowledge, DealerNotes, PlayerKnowledge}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.outcome.Outcome.{DealerWins, GameOver, PlayerWins, Reset}
import com.zkerriga.buckshot.game.events.{ItemUse, Used}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.items.Slot
import com.zkerriga.buckshot.game.state.partitipant.Side.Player
import com.zkerriga.types.Chance

case class PlayerUsed(item: FullItemUse, on: Slot, viaAdrenaline: Option[Slot])

object PlayerUsed:
  def execute(state: GameState, used: PlayerUsed): V[DealerWins.type | ContinuableOutcome | GameState] =
    Used
      .execute(
        state.public,
        Used(
          actor = Player,
          item = used.item.toPublic,
          on = used.on,
          viaAdrenalineOn = used.viaAdrenaline,
        ),
      )
      .flatMap:
        case DealerWins => DealerWins.ok
        case win: PlayerWins =>
          ContinuableOutcome.WinDetails(win, updateNotes(state.hidden.dealer.notes, used).slotGroups).ok
        case reset: Reset =>
          ContinuableOutcome.ResetDetails(reset, updateNotes(state.hidden.dealer.notes, used).slotGroups).ok
        case table: TableState =>
          for {
            dealerKnowledge <- updateDealer(state.public, table, used, state.hidden.dealer)
          } yield GameState(
            public = table,
            hidden = PrivateStates(
              dealer = dealerKnowledge,
              player = updatePlayer(used.item, state.hidden.player),
            ),
          )

  private def updateDealer(
    old: TableState,
    table: TableState,
    used: PlayerUsed,
    knowledge: DealerKnowledge,
  ): V[DealerKnowledge] =
    for {
      belief <- used.item match {
        case FullItemUse.MagnifyingGlass(shell) =>
          knowledge.belief.conditioning: revealed =>
            Chance.certainUnless(missOnGlassReveal(revealed, table.shotgun, shell))

        case FullItemUse.BurnerPhone(revealed) =>
          revealed.fold(knowledge.belief.ok): (shell, at) =>
            knowledge.belief.conditioning: revealed =>
              Chance.certainUnless(missOnPhoneReveal(revealed, table.shotgun, shell, at))

        case FullItemUse.Beer(out) =>
          knowledge.belief
            .conditioning: revealed =>
              Chance.certainUnless(missOnShellOut(revealed, old = old.shotgun, updated = table.shotgun, out = out))
            .map(_.update(_.afterShellOut))

        case _ => knowledge.belief.ok
      }
    } yield DealerKnowledge(
      belief = belief,
      notes = updateNotes(knowledge.notes, used),
    )

  private def updateNotes(notes: DealerNotes, used: PlayerUsed): DealerNotes =
    used.viaAdrenaline match
      case Some(_) => notes.withoutItemOn(used.on)
      case None => notes

  private def updatePlayer(item: FullItemUse, knowledge: PlayerKnowledge): PlayerKnowledge =
    item match
      case FullItemUse.MagnifyingGlass(revealed) => knowledge.knowing(revealed, Shell1)
      case FullItemUse.BurnerPhone(revealed) =>
        revealed.fold(knowledge): (revealed, at) =>
          knowledge.knowing(revealed, at)
      case FullItemUse.Beer(out) => knowledge.afterShellOut
      case _ => knowledge
