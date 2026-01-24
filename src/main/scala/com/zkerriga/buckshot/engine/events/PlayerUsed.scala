package com.zkerriga.buckshot.engine.events

import cats.syntax.all.*
import com.zkerriga.buckshot.engine.DealerBeliefChecks.{missOnGlassReveal, missOnPhoneReveal, missOnShellOut}
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerNotes, PlayerKnowledge}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.engine.{BeliefState, EngineError}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Used
import com.zkerriga.buckshot.game.events.outcome.Outcome.DealerWins
import com.zkerriga.buckshot.game.events.outcome.StateError
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.items.Slot
import com.zkerriga.buckshot.game.state.partitipant.Side.Player
import com.zkerriga.types.Chance
import com.zkerriga.types.boundaries.optional
import com.zkerriga.types.boundaries.optional.*
import com.zkerriga.types.steps.ResultExtension.*

case class PlayerUsed(item: FullItemUse, on: Slot, viaAdrenaline: Option[Slot])

object PlayerUsed:
  def execute(
    state: GameState,
    used: PlayerUsed,
  )(using Raise[StateError | EngineError]): DealerWins | ContinuableOutcome | GameState =
    exec(state, used): table =>
      optional:
        updateBelief(used, conditionBelief(state.public, table, used, state.hidden.dealer.belief).?)

  def executeSimple(
    state: GameState,
    used: PlayerUsed,
  )(using Raise[StateError | EngineError]): DealerWins | ContinuableOutcome | GameState =
    exec(state, used): _ =>
      updateBelief(used, state.hidden.dealer.belief).some

  private def exec(state: GameState, used: PlayerUsed)(using Raise[StateError | EngineError]) =
    val event = Used(actor = Player, item = used.item.toPublic, on = used.on, viaAdrenalineOn = used.viaAdrenaline)
    Execute.public(state, event)(Used.execute)(
      win => ContinuableOutcome.WinDetails(win, updateNotes(state.hidden.dealer.notes, used).slotGroups),
      reset => ContinuableOutcome.ResetDetails(reset, updateNotes(state.hidden.dealer.notes, used).slotGroups),
    )(
      updatePlayer(used.item, state.hidden.player),
      updateNotes(state.hidden.dealer.notes, used),
    )

  private def conditionBelief(
    old: TableState,
    table: TableState,
    used: PlayerUsed,
    belief: BeliefState[Revealed],
  ): Option[BeliefState[Revealed]] =
    used.item match
      case FullItemUse.MagnifyingGlass(shell) =>
        belief.conditioning: revealed =>
          Chance.certainUnless(missOnGlassReveal(revealed, table.shotgun, shell))
      case FullItemUse.BurnerPhone(revealed) =>
        revealed.fold(Some(belief)): (shell, at) =>
          belief.conditioning: revealed =>
            Chance.certainUnless(missOnPhoneReveal(revealed, table.shotgun, shell, at))
      case FullItemUse.Beer(out) =>
        belief.conditioning: revealed =>
          Chance.certainUnless(missOnShellOut(revealed, old = old.shotgun, updated = table.shotgun, out = out))
      case _ => Some(belief)

  private def updateBelief(used: PlayerUsed, belief: BeliefState[Revealed]): BeliefState[Revealed] =
    used.item match
      case FullItemUse.Beer(out) => belief.update(_.afterShellOut)
      case _ => belief

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
