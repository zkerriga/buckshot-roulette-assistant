package com.zkerriga.buckshot.engine.events

import cats.data.NonEmptySeq
import com.zkerriga.buckshot.engine.DealerBeliefChecks.missOnShellOut
import com.zkerriga.buckshot.engine.EngineError.*
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerKnowledge, DealerNotes, PlayerKnowledge}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.engine.{Distribution, ShellChances}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.outcome.Outcome.{DealerWins, GameOver, PlayerWins, Reset}
import com.zkerriga.buckshot.game.events.{ItemUse, Used}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.items.Slot
import com.zkerriga.buckshot.game.state.partitipant.Items.ItemOn
import com.zkerriga.buckshot.journal.AppLog.Logging
import com.zkerriga.types.{Chance, Nat}

case class DealerUsed(item: ItemUse, on: Slot, viaAdrenaline: Option[Slot])

object DealerUsed extends Logging:
  def execute(state: GameState, used: DealerUsed): V[DealerWins.type | ContinuableOutcome | GameState] =
    Used
      .execute(
        state.public,
        Used(actor = Dealer, item = used.item, on = used.on, viaAdrenalineOn = used.viaAdrenaline),
      )
      .flatMap:
        case DealerWins => DealerWins.ok
        case win: PlayerWins =>
          ContinuableOutcome.WinDetails(win, updateNotes(state.hidden.dealer.notes, used).slotGroups).ok
        case reset: Reset =>
          ContinuableOutcome.ResetDetails(reset, updateNotes(state.hidden.dealer.notes, used).slotGroups).ok
        case table: TableState =>
          for {
            dealerKnowledge <- updateDealer(
              old = state.public,
              table = table,
              used,
              state.hidden.dealer,
              state.hidden.player,
            )
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
    used: DealerUsed,
    knowledge: DealerKnowledge,
    playerKnowledge: PlayerKnowledge,
  ): V[DealerKnowledge] =
    for {
      adjusted <- knowledge.belief.conditioning(condition(old = old, oldNotes = knowledge.notes, table = table, used))
      belief = used.item match {
        case ItemUse.MagnifyingGlass =>
          adjusted.transform: revealed =>
            shellAt(table, player = playerKnowledge, dealer = revealed, at = Shell1).map: shell =>
              revealed.revealed(shell, Shell1)

        case ItemUse.BurnerPhone =>
          adjusted.transform: revealed =>
            (for
              options <- table.shotgun.total minus Nat[1]
              positions <- NonEmptySeq.fromSeq(
                Seq(Shell2, Shell3, Shell4, Shell5, Shell6, Shell7, Shell8).take(options),
              )
            yield positions).fold(Distribution.deterministic(revealed)): positions =>
              for
                seqNr <- Distribution.weighted(positions.map(Nat[1] -> _))
                shell <- shellAt(table, player = playerKnowledge, dealer = revealed, at = seqNr)
              yield revealed.revealed(shell, seqNr)

        case ItemUse.Beer(out) => adjusted.update(_.afterShellOut)
        case _ => adjusted
      }
    } yield DealerKnowledge(
      belief = belief,
      notes = updateNotes(knowledge.notes, used),
    )

  private def updateNotes(notes: DealerNotes, used: DealerUsed): DealerNotes =
    val adjusted = used.viaAdrenaline match
      case Some(_) => notes
      case None => notes.withoutItemOn(used.on)
    used.item match
      case _: ItemUse.Meds => adjusted.usingMeds
      case _ => adjusted

  private def shellAt(table: TableState, player: PlayerKnowledge, dealer: Revealed, at: SeqNr): Distribution[Shell] =
    ShellChances.shellAt(table.shotgun, player.revealed, dealer, at)

  private def condition(old: TableState, oldNotes: DealerNotes, table: TableState, used: DealerUsed)(
    revealed: Revealed,
  ): Chance =
    val beerMiss = used.item match
      case ItemUse.Beer(out) => missOnShellOut(revealed, old = old.shotgun, updated = table.shotgun, out = out)
      case _ => false

    if beerMiss then Chance.NoChance
    else {
      val itemOn = ItemOn(used.item.toItem, used.on)
      val realAction = used.viaAdrenaline match {
        case Some(_) => DealerAi.Action.Steal(itemOn)
        case None => DealerAi.Action.Use(itemOn)
      }
      val prediction = DealerAi.next(old, oldNotes, revealed)
      prediction.chanceOf(realAction)
    }

  private def updatePlayer(item: ItemUse, knowledge: PlayerKnowledge): PlayerKnowledge =
    item match
      case _: ItemUse.Beer => knowledge.afterShellOut
      case _ => knowledge
