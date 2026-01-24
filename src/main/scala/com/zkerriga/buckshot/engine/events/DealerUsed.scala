package com.zkerriga.buckshot.engine.events

import cats.data.NonEmptySeq
import cats.syntax.all.*
import com.zkerriga.buckshot.engine.DealerBeliefChecks.missOnShellOut
import com.zkerriga.buckshot.engine.EngineError.*
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerNotes, PlayerKnowledge}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.engine.{BeliefState, Distribution, ShellChances}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.outcome.Outcome.DealerWins
import com.zkerriga.buckshot.game.events.{ItemUse, Used}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.items.Slot
import com.zkerriga.buckshot.game.state.partitipant.Items.ItemOn
import com.zkerriga.types.boundaries.optional
import com.zkerriga.types.boundaries.optional.*
import com.zkerriga.types.{Chance, Nat}

case class DealerUsed(item: ItemUse, on: Slot, viaAdrenaline: Option[Slot])

object DealerUsed:
  def execute(state: GameState, used: DealerUsed): V[DealerWins | ContinuableOutcome | GameState] =
    exec(state, used): table =>
      optional:
        updateBelief(
          state.hidden.player,
          used,
          table,
          conditionBelief(
            old = state.public,
            state.hidden.dealer.notes,
            table = table,
            used,
            state.hidden.dealer.belief,
          ).?,
        )

  def executeSimple(state: GameState, used: DealerUsed): V[DealerWins | ContinuableOutcome | GameState] =
    exec(state, used): table =>
      updateBelief(state.hidden.player, used, table, state.hidden.dealer.belief).some

  private def exec(state: GameState, used: DealerUsed) =
    val event = Used(actor = Dealer, item = used.item, on = used.on, viaAdrenalineOn = used.viaAdrenaline)
    Execute.public(state, event)(Used.execute)(
      win => ContinuableOutcome.WinDetails(win, updateNotes(state.hidden.dealer.notes, used).slotGroups),
      reset => ContinuableOutcome.ResetDetails(reset, updateNotes(state.hidden.dealer.notes, used).slotGroups),
    )(
      player = updatePlayer(used.item, state.hidden.player),
      notes = updateNotes(state.hidden.dealer.notes, used),
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

  private def conditionBelief(
    old: TableState,
    oldNotes: DealerNotes,
    table: TableState,
    used: DealerUsed,
    belief: BeliefState[Revealed],
  ): Option[BeliefState[Revealed]] =
    belief.conditioning: revealed =>
      val beerMiss = used.item match
        case ItemUse.Beer(out) => missOnShellOut(revealed, old = old.shotgun, updated = table.shotgun, out = out)
        case _ => false

      if beerMiss then Chance.NoChance
      else
        val itemOn = ItemOn(used.item.toItem, used.on)
        val realAction = used.viaAdrenaline match {
          case Some(_) => DealerAi.Action.Steal(itemOn)
          case None => DealerAi.Action.Use(itemOn)
        }
        val prediction = DealerAi.next(old, oldNotes, revealed)
        prediction.chanceOf(realAction)

  private def updateBelief(
    playerKnowledge: PlayerKnowledge,
    used: DealerUsed,
    table: TableState,
    belief: BeliefState[Revealed],
  ): BeliefState[Revealed] =
    used.item match
      case ItemUse.MagnifyingGlass =>
        belief.transform: revealed =>
          shellAt(table, player = playerKnowledge, dealer = revealed, at = Shell1).map: shell =>
            revealed.revealed(shell, Shell1)

      case ItemUse.BurnerPhone =>
        belief.transform: revealed =>
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

      case ItemUse.Beer(out) => belief.update(_.afterShellOut)
      case _ => belief

  private def updatePlayer(item: ItemUse, knowledge: PlayerKnowledge): PlayerKnowledge =
    item match
      case _: ItemUse.Beer => knowledge.afterShellOut
      case _ => knowledge
