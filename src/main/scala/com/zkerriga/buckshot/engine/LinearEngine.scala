package com.zkerriga.buckshot.engine

import cats.effect.cps.*
import cats.effect.{Concurrent, Ref}
import com.zkerriga.buckshot.engine.LinearEngine.*
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.V
import com.zkerriga.buckshot.game.events.outcome.Outcome.*
import com.zkerriga.buckshot.game.events.{Shot, Used}
import com.zkerriga.buckshot.game.state.GameState

final class LinearEngine[F[_]] private (state: Ref[F, LinearState]):
  def execute(event: Event): F[Either[Error, GameOver | Reset | GameState]] =
    state.modify: state =>
      state.current match
        case outcome: (GameOver | Reset) => (state, Left(CannotBeContinued(outcome)))
        case current: GameState =>
          current.execute(event) match
            case Left(error) => (state, Left(error))
            case Right(outcome) => (state.changed(outcome, event), Right(outcome))

object LinearEngine:
  type Event = Shot | Used

  case class CannotBeContinued(outcome: GameOver | Reset)
  type Error = ErrorMsg | CannotBeContinued

  extension (game: GameState)
    private def execute(event: Event): V[GameOver | Reset | GameState] =
      event match
        case shot: Shot => Shot.execute(game, shot)
        case used: Used => Used.execute(game, used)

  private case class LinearState(
    initial: GameState,
    events: Vector[Event],
    current: GameOver | Reset | GameState,
  )
  private object LinearState:
    extension (state: LinearState)
      def changed(updated: GameOver | Reset | GameState, by: Event): LinearState =
        state.copy(
          events = state.events :+ by,
          current = updated,
        )

  def init[F[_]: Concurrent](initial: GameState): F[LinearEngine[F]] =
    async[F]:
      val state = LinearState(initial, Vector.empty, initial)
      LinearEngine(Ref.of[F, LinearState](state).await)
