package com.zkerriga.buckshot.engine

import com.zkerriga.buckshot.engine.Engine.*
import com.zkerriga.buckshot.engine.events.{DealerShot, DealerUsed, PlayerShot, PlayerUsed}
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg
import com.zkerriga.buckshot.game.events.outcome.Outcome.{DealerWins, GameOver, PlayerWins, Reset}
import com.zkerriga.buckshot.game.events.{Shot, Used, outcome}
import com.zkerriga.buckshot.game.state.items.RegularItem
import com.zkerriga.buckshot.game.state.partitipant.Side.{Dealer, Player}
import com.zkerriga.buckshot.game.state.partitipant.{Items, Side}
import com.zkerriga.buckshot.game.state.shotgun.Shotgun
import com.zkerriga.buckshot.game.state.{TableState, partitipant}
import com.zkerriga.buckshot.journal.AppLog.Logging
import com.zkerriga.types.Ref

class Engine(state: Ref[GameOver | Reset | GameState]):
  def getState: Either[ErrorText, GameState] =
    state.get match
      case outcome: (GameOver | Reset) => "no state".error
      case state: GameState => state.ok

  def process(event: Engine.Event): Either[ErrorText, EventReply] =
    state.modify:
      case outcome: (GameOver | Reset) => (outcome, "game over".error)
      case state: GameState =>
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
              case ErrorMsg.MissingItem(item) =>
                log.warn(s"missing item $item for $event on $state")
                (state, s"missing item $item".error)
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
                    DealerPrediction(Distribution.deterministic(Vector(Action.Shoot(Player)))) // todo: implement
                (newState, EventReply.NewState(newState, dealer = dealerPrediction).ok)

  def continue(shells: Shotgun.ShellDistribution, dealer: Items, player: Items): Either[ErrorText, GameState] =
    state.modify:
      case over: GameOver => (over, "game over".error)
      case state: GameState => (state, "in progress".error)
      case reset: Reset =>
        val state = GameState.initial(TableState.from(reset, shells, dealer = dealer, player = player))
        log.info(s"state reinitialized to $state")
        (state, state.ok)

object Engine extends Logging:
  type Event = Shot[Side] | Used[Dealer.type] | PlayerUsed
  private def execute(state: GameState, event: Event): Either[ErrorMsg | EngineError, GameOver | Reset | GameState] =
    event match
      case Shot(Player, target, shell) =>
        log.info(s"player shot $target with $shell")
        PlayerShot.execute(state, Shot(Player, target, shell))
      case Shot(Dealer, target, shell) =>
        log.info(s"dealer shot $target with $shell")
        DealerShot.execute(state, Shot(Dealer, target, shell))
      case used @ Used(Dealer, item, steal) =>
        log.info(s"dealer used $item $steal")
        DealerUsed.execute(state, used)
      case used @ PlayerUsed(item, steal) =>
        log.info(s"player used $item $steal")
        PlayerUsed.execute(state, used)

  type ErrorText = String

  extension (text: ErrorText) private[Engine] def error = Left(text)
  extension [A](value: A) private[Engine] def ok = Right(value)

  enum EventReply:
    case NewState(state: GameState, dealer: Option[DealerPrediction])
    case GameOver(winner: Side)
    case ShotgunReset(reset: Reset)

  enum Action:
    case Shoot(target: Side)
    case Use(item: RegularItem, steal: Boolean)

  case class DealerPrediction(actions: Distribution[Vector[Action]])

  def start(table: TableState): Engine =
    log.info(s"starting engine with $table")
    Engine(Ref.of(GameState.initial(table)))
