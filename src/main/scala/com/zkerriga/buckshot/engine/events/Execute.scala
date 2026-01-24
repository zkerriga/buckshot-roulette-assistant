package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.engine.EngineError.BadConditioning
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerKnowledge, DealerNotes, PlayerKnowledge}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.engine.{BeliefState, EngineError}
import com.zkerriga.buckshot.game.events.Shot
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg
import com.zkerriga.buckshot.game.events.outcome.Outcome.{DealerWins, PlayerWins, Reset}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.types.boundaries.eithers
import com.zkerriga.types.boundaries.eithers.*

object Execute:
  def shot(state: GameState, event: Shot)(player: PlayerKnowledge, notes: DealerNotes)(
    belief: TableState => Option[BeliefState[Revealed]],
  ): EngineError.V[DealerWins | ContinuableOutcome | GameState] =
    public(state, event)(Shot.execute)(
      win => ContinuableOutcome.WinDetails(win, state.hidden.dealer.notes.slotGroups),
      reset => ContinuableOutcome.ResetDetails(reset, state.hidden.dealer.notes.slotGroups),
    )(player, notes)(belief)

  def public[Event](state: GameState, event: Event)(
    handler: (TableState, Event) => ErrorMsg.V[DealerWins | PlayerWins | Reset | TableState],
  )(
    onWin: PlayerWins => ContinuableOutcome.WinDetails,
    onReset: Reset => ContinuableOutcome.ResetDetails,
  )(
    player: PlayerKnowledge,
    notes: DealerNotes,
  )(
    belief: TableState => Option[BeliefState[Revealed]],
  ): EngineError.V[DealerWins | ContinuableOutcome | GameState] =
    eithers:
      handler(state.public, event).? match
        case DealerWins => DealerWins
        case win: PlayerWins => onWin(win)
        case reset: Reset => onReset(reset)
        case table: TableState =>
          GameState(
            public = table,
            hidden = PrivateStates(
              dealer = DealerKnowledge(
                belief = belief(table).toRight(BadConditioning).?,
                notes = notes,
              ),
              player = player,
            ),
          )
