package com.zkerriga.buckshot.search

import cats.data.NonEmptyList
import cats.kernel.Semigroup
import com.zkerriga.buckshot.engine.{Distribution, EngineError, ShellChances}
import com.zkerriga.buckshot.engine.EngineError.*
import com.zkerriga.buckshot.engine.events.{ContinuableOutcome, PlayerShot}
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg
import com.zkerriga.buckshot.game.events.outcome.Outcome.DealerWins
import com.zkerriga.buckshot.game.state.partitipant
import com.zkerriga.types.Chance

object Search:
  enum Action:
    case Use(item: ItemOn)
    case Steal(item: ItemOn)
    case Shoot(target: Side)

  type Evaluation = Double

  private def options(table: TableState): NonEmptyList[Action] =
    NonEmptyList(
      Action.Shoot(Dealer),
      List(
        if table.player.hasAdrenaline then table.dealer.items.positioned.map(Action.Steal(_)) else Nil,
        table.player.items.positioned.map(Action.Use(_)),
        List(Action.Shoot(Player)),
      ).flatten,
    )

  private case class WrongCalculation(error: ErrorMsg | EngineError) extends Throwable
  private object WrongCalculation:
    extension [A](value: V[A])
      def orThrow: A =
        value match
          case Left(error) => throw WrongCalculation(error)
          case Right(a) => a

  import WrongCalculation.*

  extension (evaluations: Distribution[Evaluation])
    private def mathExpectation: Evaluation =
      evaluations.sumMerged: (chance, evaluation) =>
        chance * evaluation

  private def evaluateOutcome(outcome: DealerWins.type | ContinuableOutcome | GameState, depth: Int): Evaluation =
    outcome match
      case DealerWins => ??? // todo: a constant
      case ContinuableOutcome.WinDetails(win = win) => ??? // todo: win item evaluation
      case ContinuableOutcome.ResetDetails(reset = reset) => ??? // todo: reset details evaluation
      case next: GameState =>
        if depth > 0 then search(next, depth - 1).evaluation
        else ??? // todo: heuristic state evaluation

  private def evaluateAction(state: GameState, action: Action, depth: Int): Evaluation =
    action match {
      case Action.Use(item) => ???
      case Action.Steal(item) => ???
      case Action.Shoot(target) =>
        val shellChances = for {
          dealerRevealed <- state.hidden.dealer.belief.getDistribution
          shell <- ShellChances.shellAt(
            shotgun = state.shotgun,
            player = state.hidden.player.revealed,
            dealer = dealerRevealed,
            at = Shell1,
          )
        } yield shell
        val evaluatedOutcomes = shellChances.deduplicate.map: shell =>
          // todo: how to avoid conditioning inside of event execution?
          // todo: maybe extract conditioning into a separate pre-execute step?
          val outcome = PlayerShot.execute(state, PlayerShot(target, shell)).orThrow
          evaluateOutcome(outcome, depth)
        evaluatedOutcomes.mathExpectation
    }

  private def search(state: GameState, depth: Int): (action: Action, evaluation: Evaluation) =
    options(state.public)
      .map: action =>
        (action = action, evaluation = evaluateAction(state, action, depth))
      .toList
      .maxBy(_.evaluation)

  def best(state: GameState): V[(action: Action, evaluation: Evaluation)] =
    try search(state, depth = 1000).ok
    catch {
      case WrongCalculation(error) => Left(error)
    }
