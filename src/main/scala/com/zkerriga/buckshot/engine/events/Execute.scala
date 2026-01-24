package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.engine.EngineError.BadConditioning
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerKnowledge, DealerNotes, PlayerKnowledge}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.engine.{BeliefState, EngineError}
import com.zkerriga.buckshot.game.events.Shot
import com.zkerriga.buckshot.game.events.outcome.Outcome.{DealerWins, PlayerWins, Reset}
import com.zkerriga.buckshot.game.events.outcome.StateError
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.types.boundaries.eithers
import com.zkerriga.types.boundaries.eithers.*
import com.zkerriga.types.steps.ResultExtension.*
import steps.result.Result
import steps.result.ScalaConverters.*

object Execute:
  def shot(state: GameState, event: Shot)(player: PlayerKnowledge, notes: DealerNotes)(
    belief: TableState => Option[BeliefState[Revealed]],
  )(using Raise[StateError | EngineError]): DealerWins | ContinuableOutcome | GameState =
    public(state, event)(Shot.execute)(
      win => ContinuableOutcome.WinDetails(win, state.hidden.dealer.notes.slotGroups),
      reset => ContinuableOutcome.ResetDetails(reset, state.hidden.dealer.notes.slotGroups),
    )(player, notes)(belief)

  def public[Event](state: GameState, event: Event)(
    handler: Raise[StateError] ?=> (TableState, Event) => DealerWins | PlayerWins | Reset | TableState,
  )(
    onWin: PlayerWins => ContinuableOutcome.WinDetails,
    onReset: Reset => ContinuableOutcome.ResetDetails,
  )(
    player: PlayerKnowledge,
    notes: DealerNotes,
  )(
    belief: TableState => Option[BeliefState[Revealed]],
  )(using
    Raise[StateError | EngineError],
  ): DealerWins | ContinuableOutcome | GameState =
    handler(state.public, event) match
      case DealerWins => DealerWins
      case win: PlayerWins => onWin(win)
      case reset: Reset => onReset(reset)
      case table: TableState =>
        GameState(
          public = table,
          hidden = PrivateStates(
            dealer = DealerKnowledge(
              belief = belief(table).getOrRaise(BadConditioning),
              notes = notes,
            ),
            player = player,
          ),
        )
