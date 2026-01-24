package com.zkerriga.buckshot.engine.events

import cats.syntax.all.*
import com.zkerriga.buckshot.engine.DealerBeliefChecks.missOnShellOut
import com.zkerriga.buckshot.engine.EngineError.*
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.state.PrivateStates.DealerKnowledge
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.engine.{BeliefState, EngineError}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Shot
import com.zkerriga.buckshot.game.events.outcome.Outcome.DealerWins
import com.zkerriga.buckshot.game.events.outcome.StateError
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.types.Chance
import com.zkerriga.types.boundaries.optional
import com.zkerriga.types.boundaries.optional.*
import com.zkerriga.types.steps.ResultExtension.*

case class DealerShot(target: Side, shell: Shell)

object DealerShot:
  def execute(
    state: GameState,
    shot: DealerShot,
  )(using Raise[StateError | EngineError]): DealerWins | ContinuableOutcome | GameState =
    exec(state, shot): table =>
      optional:
        updateBelief(conditionBelief(old = state.public, table = table, shot, state.hidden.dealer).?)

  def executeSimple(
    state: GameState,
    shot: DealerShot,
  )(using Raise[StateError | EngineError]): DealerWins | ContinuableOutcome | GameState =
    exec(state, shot): _ =>
      updateBelief(state.hidden.dealer.belief).some

  private def exec(state: GameState, shot: DealerShot)(using Raise[StateError | EngineError]) =
    val event = Shot(actor = Dealer, target = shot.target, shell = shot.shell)
    Execute.shot(state, event)(
      player = state.hidden.player.afterShellOut,
      notes = state.hidden.dealer.notes.copy(usedMeds = false),
    )

  private def conditionBelief(
    old: TableState,
    table: TableState,
    shot: DealerShot,
    knowledge: DealerKnowledge,
  ): Option[BeliefState[Revealed]] =
    knowledge.belief.conditioning: revealed =>
      if missOnShellOut(revealed, old = old.shotgun, updated = table.shotgun, out = shot.shell) then Chance.NoChance
      else
        val realAction = DealerAi.Action.Shoot(shot.target)
        val prediction = DealerAi.next(old, knowledge.notes, revealed)
        prediction.chanceOf(realAction)

  private def updateBelief(belief: BeliefState[Revealed]): BeliefState[Revealed] =
    belief.update(_.afterShellOut)
