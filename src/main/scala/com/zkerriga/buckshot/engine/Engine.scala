package com.zkerriga.buckshot.engine

import com.zkerriga.buckshot.engine.Engine.*
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.events.*
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg
import com.zkerriga.buckshot.game.events.outcome.Outcome.DealerWins
import com.zkerriga.buckshot.journal.AppLog.Logging
import com.zkerriga.types.Ref

class Engine(state: Ref[DealerWins.type | ContinuableOutcome | GameState]):
  def getState: Either[ErrorText, GameState] =
    state.get match
      case outcome: (DealerWins.type | ContinuableOutcome) => "no state".error
      case state: GameState => state.ok

  def process(event: Engine.Event): Either[ErrorText, EventReply] =
    state.modify:
      case outcome: (DealerWins.type | ContinuableOutcome) => (outcome, "game over".error)
      case state: GameState =>
        log.info(s"processing event $event")
        execute(state, event) match
          case Left(error) =>
            error match
              case EngineError.BadConditioning =>
                log.error(s"critical problem with conditioning, while processing $event on $state")
                (state, "critical issue".error)
              case ErrorMsg.WrongTurn =>
                log.warn(s"wrong turn for $event on $state")
                (state, "wrong turn".error)
              case ErrorMsg.ShotgunStateMismatch =>
                log.error(s"shotgun state mismatch, while processing $event on $state")
                (state, "critical issue".error)
              case ErrorMsg.MissingItem =>
                log.warn(s"missing item for $event on $state")
                (state, s"missing item".error)
              case ErrorMsg.SawAlreadyUsed =>
                log.warn(s"saw already used for $event on $state")
                (state, "saw already used".error)
              case ErrorMsg.HandsAlreadyCuffed =>
                log.warn(s"hands already cuffed for $event on $state")
                (state, "hands already cuffed".error)

          case Right(outcome) =>
            outcome match
              case DealerWins =>
                log.info("game is lost")
                (DealerWins, EventReply.GameOver(None).ok)
              case win: ContinuableOutcome.WinDetails =>
                log.info(s"game is over with $win")
                (win, EventReply.GameOver(Some(win)).ok)
              case reset: ContinuableOutcome.ResetDetails =>
                log.info("shotgun is resetting")
                (reset, EventReply.ShotgunReset(reset).ok)
              case newState: GameState =>
                log.debug(s"state chanced to $newState")
                (newState, EventReply.NewState(newState).ok)

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
  ): Either[ErrorMsg | EngineError, DealerWins.type | ContinuableOutcome | GameState] =
    event match
      case e: PlayerShot => PlayerShot.execute(state, e)
      case e: DealerShot => DealerShot.execute(state, e)
      case e: DealerUsed => DealerUsed.execute(state, e)
      case e: PlayerUsed => PlayerUsed.execute(state, e)

  type ErrorText = String

  extension (text: ErrorText) private[Engine] def error = Left(text)
  extension [A](value: A) private[Engine] def ok = Right(value)

  enum EventReply:
    case NewState(state: GameState)
    case GameOver(outcome: Option[ContinuableOutcome.WinDetails])
    case ShotgunReset(reset: ContinuableOutcome.ResetDetails)

  def start(state: GameState): Engine =
    log.info(s"starting engine with $state")
    Engine(Ref.of(state))
