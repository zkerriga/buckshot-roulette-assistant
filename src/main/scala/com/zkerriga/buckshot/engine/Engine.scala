package com.zkerriga.buckshot.engine

import cats.Eq
import com.zkerriga.buckshot.engine.Engine.*
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.events.{DealerShot, DealerUsed, PlayerShot, PlayerUsed}
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerKnowledge, DealerNotes, PlayerKnowledge}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg
import com.zkerriga.buckshot.game.events.outcome.Outcome.{DealerWins, GameOver, PlayerWins, Reset}
import com.zkerriga.buckshot.game.events.{Shot, Used, outcome}
import com.zkerriga.buckshot.journal.AppLog.Logging
import com.zkerriga.types.{Nat, Ref}

class Engine(state: Ref[GameOver | Reset | GameState]):
  def getState: Either[ErrorText, GameState] =
    state.get match
      case outcome: (GameOver | Reset) => "no state".error
      case state: GameState => state.ok

  def process(event: Engine.Event): Either[ErrorText, EventReply] =
    state.modify:
      case outcome: (GameOver | Reset) => (outcome, "game over".error)
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
              case gameOver: GameOver =>
                val winner = gameOver match
                  case DealerWins => Dealer
                  case PlayerWins(_, _) => Player
                log.info(s"game is over $winner")
                (gameOver, EventReply.GameOver(winner).ok)
              case reset: Reset =>
                log.info("shotgun is resetting")
                (reset, EventReply.ShotgunReset(reset).ok)
              case newState: GameState =>
                log.debug(s"state chanced to $newState")
                val dealerPrediction =
                  Option.when(newState.turn == Dealer):
                    DealerPrediction.on(newState)
                (newState, EventReply.NewState(newState, dealer = dealerPrediction).ok)

  def continue(shells: Shotgun.ShellDistribution, dealer: Items, player: Items): Either[ErrorText, GameState] =
    state.modify:
      case over: GameOver => (over, "game over".error)
      case state: GameState => (state, "in progress".error)
      case reset: Reset =>
        val state = ???
        log.info(s"state reinitialized to $state")
        (state, state.ok)

object Engine extends Logging:
  type Event = DealerShot | PlayerShot | DealerUsed | PlayerUsed
  private def execute(state: GameState, event: Event): Either[ErrorMsg | EngineError, GameOver | Reset | GameState] =
    event match
      case e: PlayerShot => PlayerShot.execute(state, e)
      case e: DealerShot => DealerShot.execute(state, e)
      case e: DealerUsed => DealerUsed.execute(state, e)
      case e: PlayerUsed => PlayerUsed.execute(state, e)

  type ErrorText = String

  extension (text: ErrorText) private[Engine] def error = Left(text)
  extension [A](value: A) private[Engine] def ok = Right(value)

  enum EventReply:
    case NewState(state: GameState, dealer: Option[DealerPrediction])
    case GameOver(winner: Side)
    case ShotgunReset(reset: Reset)

  case class DealerPrediction(possible: Distribution[DealerAi.Action])
  object DealerPrediction:
    def on(state: GameState): DealerPrediction = DealerPrediction {
      state.hidden.dealer.belief.getDistribution
        .flatMap(DealerAi.next(state.public, state.hidden.dealer.notes, _)) // todo: add cache
        .deduplicate(using Eq.fromUniversalEquals) // todo: fix Eq
    }

  def start(table: TableState): Engine =
    log.info(s"starting engine with $table")
    val state = GameState(
      public = table,
      hidden = PrivateStates(
        dealer = DealerKnowledge(
          belief = BeliefState.deterministic(Revealed.Nothing),
          notes = DealerNotes(
            usedMeds = false,
            slotGroups = List(table.dealer.items.positioned.map(_.on).toSet),
          ),
        ),
        player = PlayerKnowledge(Revealed.Nothing),
      ),
    )
    Engine(Ref.of(state))
