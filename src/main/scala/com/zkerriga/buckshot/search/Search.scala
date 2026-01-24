package com.zkerriga.buckshot.search

import cats.data.NonEmptyList
import cats.syntax.all.*
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.events.*
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.engine.{Distribution, EngineError, GameChances}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.ItemUse
import com.zkerriga.buckshot.game.events.outcome.Outcome.DealerWins
import com.zkerriga.buckshot.game.events.outcome.StateError
import com.zkerriga.buckshot.game.state
import com.zkerriga.buckshot.game.state.partitipant
import com.zkerriga.types.steps.ResultExtension.*
import steps.result.Result

object Search:
  enum Action:
    case Use(item: ItemOn)
    case Steal(item: ItemOn)
    case Shoot(target: Side)

  type Error = StateError | EngineError

  private def options(table: TableState): NonEmptyList[Action] =
    val dropRestricted: Seq[ItemOn] => Seq[ItemOn] =
      if table.shotgun.effects.damage == Damage.Double then _.filterNot(_.item == Saw)
      else identity
    NonEmptyList(
      Action.Shoot(Dealer),
      List(
        if table.player.hasAdrenaline then dropRestricted(table.dealer.items.positioned).map(Action.Steal(_)) else Nil,
        dropRestricted(table.player.items.positioned).map(Action.Use(_)),
        List(Action.Shoot(Player)),
      ).flatten,
    )

  import Evaluation.*

  extension (state: GameState)
    private def itemUseOf(item: RegularItem): Distribution[ItemUse] =
      item match
        case Handcuffs => Distribution.deterministic(ItemUse.Handcuffs)
        case MagnifyingGlass => Distribution.deterministic(ItemUse.MagnifyingGlass)
        case Beer =>
          GameChances
            .nextShell(state)
            .map: shell =>
              ItemUse.Beer(shell)
        case Cigarettes => Distribution.deterministic(ItemUse.Cigarettes)
        case Saw => Distribution.deterministic(ItemUse.Saw)
        case Inverter => Distribution.deterministic(ItemUse.Inverter)
        case BurnerPhone => Distribution.deterministic(ItemUse.BurnerPhone)
        case Meds =>
          GameChances.MedsGood.map: good =>
            ItemUse.Meds(good = good)

    private def fullItemUseOf(item: RegularItem): Distribution[FullItemUse] =
      item match
        case Handcuffs => Distribution.deterministic(FullItemUse.Handcuffs)
        case MagnifyingGlass =>
          GameChances
            .nextShell(state)
            .map: shell =>
              FullItemUse.MagnifyingGlass(shell)
        case Beer =>
          GameChances
            .nextShell(state)
            .map: shell =>
              FullItemUse.Beer(shell)
        case Cigarettes => Distribution.deterministic(FullItemUse.Cigarettes)
        case Saw => Distribution.deterministic(FullItemUse.Saw)
        case Inverter => Distribution.deterministic(FullItemUse.Inverter)
        case BurnerPhone =>
          GameChances
            .burnerPhonePosition(state.public)
            .map: positions =>
              for
                seqNr <- positions
                shell <- GameChances.shellAt(state, seqNr)
              yield FullItemUse.BurnerPhone((revealed = shell, at = seqNr).some)
            .getOrElse:
              Distribution.deterministic(FullItemUse.BurnerPhone(None))

        case Meds =>
          GameChances.MedsGood.map: good =>
            FullItemUse.Meds(good = good)

  private def evaluateOutcome(
    outcome: DealerWins.type | ContinuableOutcome | GameState,
    depth: Int,
  )(using Raise[Error]): Evaluation =
    outcome match
      case DealerWins => DealerWinsEvaluation
      case win: ContinuableOutcome.WinDetails => Evaluation.evaluateWin(win)
      case reset: ContinuableOutcome.ResetDetails => Evaluation.evaluateReset(reset)
      case next: GameState =>
        next.turn match
          case Player =>
            if depth > 0 then search(next, depth - 1).evaluation
            else Evaluation.evaluateState(next)
          case Dealer =>
            GameChances
              .nextDealerAction(next)
              .mathExpectation:
                case DealerAi.Action.Use(itemOn) =>
                  next
                    .itemUseOf(itemOn.item)
                    .mathExpectation: itemUse =>
                      evaluateOutcome(
                        DealerUsed.executeSimple(next, DealerUsed(itemUse, itemOn.on, viaAdrenaline = None)),
                        depth,
                      )
                case DealerAi.Action.Steal(itemOn) =>
                  next
                    .itemUseOf(itemOn.item)
                    .mathExpectation: itemUse =>
                      evaluateOutcome(
                        DealerUsed.executeSimple(
                          next,
                          DealerUsed(itemUse, itemOn.on, viaAdrenaline = next.dealer.items.adrenaline.headOption),
                        ),
                        depth,
                      )
                case DealerAi.Action.Shoot(target) =>
                  GameChances
                    .nextShell(next)
                    .mathExpectation: shell =>
                      evaluateOutcome(DealerShot.executeSimple(next, DealerShot(target, shell)), depth)

  private def evaluateAction(state: GameState, action: Action, depth: Int)(using Raise[Error]): Evaluation =
    action match
      case Action.Use(itemOn) =>
        state
          .fullItemUseOf(itemOn.item)
          .mathExpectation: itemUse =>
            evaluateOutcome(
              PlayerUsed.executeSimple(state, PlayerUsed(itemUse, itemOn.on, viaAdrenaline = None)),
              depth,
            )
      case Action.Steal(itemOn) =>
        state
          .fullItemUseOf(itemOn.item)
          .mathExpectation: itemUse =>
            evaluateOutcome(
              PlayerUsed.executeSimple(
                state,
                PlayerUsed(itemUse, itemOn.on, viaAdrenaline = state.player.items.adrenaline.headOption),
              ),
              depth,
            )
      case Action.Shoot(target) =>
        GameChances
          .nextShell(state)
          .mathExpectation: shell =>
            evaluateOutcome(PlayerShot.executeSimple(state, PlayerShot(target, shell)), depth)

  private def search(state: GameState, depth: Int)(using Raise[Error]): (action: Action, evaluation: Evaluation) =
    options(state.public)
      .map: action =>
        (action = action, evaluation = evaluateAction(state, action, depth))
      .bestBy(_.evaluation)

  def best(state: GameState): Result[(action: Action, evaluation: Evaluation), Error] =
    Result.scope:
      search(state, depth = 1000)
