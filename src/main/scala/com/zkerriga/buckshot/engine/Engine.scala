package com.zkerriga.buckshot.engine

import com.zkerriga.buckshot.engine.Engine.*
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.events.*
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.outcome.Outcome.DealerWins
import com.zkerriga.buckshot.game.events.outcome.StateError
import com.zkerriga.buckshot.journal.AppLog.Logging
import com.zkerriga.types.Ref
import com.zkerriga.types.steps.ResultExtension.*
import steps.result.Result

class Engine(state: Ref[DealerWins.type | ContinuableOutcome | GameState]):
  def getState: Result[GameState, ErrorText] =
    state.get match
      case outcome: (DealerWins | ContinuableOutcome) => "no state".asError
      case state: GameState => state.asOk

  def process(event: Engine.Event): Result[EventReply, ErrorText] =
    state.modify:
      case outcome: (DealerWins | ContinuableOutcome) => (outcome, Result.Err("game over"))
      case state: GameState =>
        log.info(s"processing event $event")
        execute(state, event) match
          case Result.Err(error) =>
            error match
              case EngineError.BadConditioning =>
                log.error(s"critical problem with conditioning, while processing $event on $state")
                (state, "critical issue".asError)
              case StateError.WrongTurn =>
                log.warn(s"wrong turn for $event on $state")
                (state, "wrong turn".asError)
              case StateError.ShotgunStateMismatch =>
                log.error(s"shotgun state mismatch, while processing $event on $state")
                (state, "critical issue".asError)
              case StateError.MissingItem =>
                log.warn(s"missing item for $event on $state")
                (state, s"missing item".asError)
              case StateError.SawAlreadyUsed =>
                log.warn(s"saw already used for $event on $state")
                (state, "saw already used".asError)
              case StateError.HandsAlreadyCuffed =>
                log.warn(s"hands already cuffed for $event on $state")
                (state, "hands already cuffed".asError)

          case Result.Ok(outcome) =>
            outcome match
              case DealerWins =>
                log.info("game is lost")
                (DealerWins, EventReply.GameOver(None).asOk)
              case win: ContinuableOutcome.WinDetails =>
                log.info(s"game is over with $win")
                (win, EventReply.GameOver(Some(win)).asOk)
              case reset: ContinuableOutcome.ResetDetails =>
                log.info("shotgun is resetting")
                (reset, EventReply.ShotgunReset(reset).asOk)
              case newState: GameState =>
                log.debug(s"state chanced to $newState")
                (newState, EventReply.NewState(newState).asOk)

  def calculateDealerPrediction(state: GameState): Distribution[DealerAi.Action] =
    state.hidden.dealer.belief.getDistribution
      .flatMap(DealerAi.next(state.public, state.hidden.dealer.notes, _)) // todo: add cache
      .deduplicate

  def calculateShellsChances(state: GameState): Seq[(SeqNr, Distribution[Shell])] =
    SeqNr.values.toSeq.take(state.shotgun.total).map { seqNr =>
      val distribution = ShellChances.consideringEverything(state, seqNr)
      seqNr -> distribution
    }

object Engine extends Logging:
  type Event = DealerShot | PlayerShot | DealerUsed | PlayerUsed
  private def execute(
    state: GameState,
    event: Event,
  ): Result[DealerWins | ContinuableOutcome | GameState, StateError | EngineError] =
    Result.scope:
      event match
        case e: PlayerShot => PlayerShot.execute(state, e)
        case e: DealerShot => DealerShot.execute(state, e)
        case e: DealerUsed => DealerUsed.execute(state, e)
        case e: PlayerUsed => PlayerUsed.execute(state, e)

  type ErrorText = String

  enum EventReply:
    case NewState(state: GameState)
    case GameOver(outcome: Option[ContinuableOutcome.WinDetails])
    case ShotgunReset(reset: ContinuableOutcome.ResetDetails)

  def start(state: GameState): Engine =
    log.info(s"starting engine with $state")
    Engine(Ref.of(state))
