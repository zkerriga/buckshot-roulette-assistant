package com.zkerriga.buckshot.search

import cats.data.NonEmptyList
import com.zkerriga.buckshot.engine.events.ContinuableOutcome
import com.zkerriga.buckshot.engine.{BeliefState, Distribution}
import com.zkerriga.buckshot.engine.state.{GameState, Revealed}
import com.zkerriga.buckshot.game.state.partitipant.{Hands, Health, Items}

opaque type Evaluation = Double

object Evaluation:
  extension [A](distribution: Distribution[A])
    def mathExpectation(eval: A => Evaluation): Evaluation =
      distribution.sumMerged: (chance, value) =>
        chance * eval(value)

  extension [A](options: NonEmptyList[A])
    def bestBy(eval: A => Evaluation): A =
      options.toList.maxBy(eval)

  val PlayerWinsEvaluation: Evaluation = 1_000_000
  val DealerWinsEvaluation: Evaluation = -PlayerWinsEvaluation
  val ResetEvaluation: Evaluation = 100_000

  val HealthPointEvaluation: Evaluation = 10_000
  val ItemAccessEvaluation: Evaluation = 1000
  val CuffedTurnEvaluation: Evaluation = 600
  val UnusableAdrenalineEvaluation: Evaluation = 400
  val RevealedShellEvaluation: Evaluation = 10

  def evaluateItems(player: Items, dealer: Items): Evaluation =
    val playerHas = player.positioned.size
    val dealerHas = dealer.positioned.size
    val playerAdrenalins = player.adrenaline.size
    val dealerAdrenalins = dealer.adrenaline.size

    val baseAccess = (playerHas - dealerHas) * ItemAccessEvaluation
    val stealing = (playerAdrenalins.min(dealerHas) - dealerAdrenalins.min(playerHas)) * ItemAccessEvaluation
    val unusableAdrenaline =
      ((playerAdrenalins - dealerHas).max(0) - (dealerAdrenalins - playerHas).max(0)) * UnusableAdrenalineEvaluation

    baseAccess + stealing + unusableAdrenaline

  def evaluateHealth(player: Health, dealer: Health): Evaluation =
    (player.asInt - dealer.asInt) * HealthPointEvaluation

  def evaluateKnowledge(player: Revealed, dealer: BeliefState[Revealed]): Evaluation =
    player.size.toDouble - dealer.getDistribution.mathExpectation(_.size)

  def evaluateHands(player: Hands, dealer: Hands): Evaluation =
    def eval(hands: Hands): Evaluation =
      hands match
        case Hands.Free => 0
        case Hands.CuffedForTwoShots => CuffedTurnEvaluation * 2
        case Hands.CuffedForOneShot => CuffedTurnEvaluation
    eval(dealer) - eval(player)

  def evaluateWin(win: ContinuableOutcome.WinDetails): Evaluation =
    PlayerWinsEvaluation + evaluateItems(player = win.win.player, dealer = win.win.dealer)

  def evaluateReset(reset: ContinuableOutcome.ResetDetails): Evaluation =
    import reset.reset.{player, dealer}
    ResetEvaluation +
      evaluateHealth(player = player.health, dealer = dealer.health) +
      evaluateItems(player = player.items, dealer = dealer.items)

  def evaluateState(state: GameState): Evaluation =
    evaluateHealth(player = state.player.health, dealer = state.dealer.health) +
      evaluateItems(player = state.player.items, dealer = state.dealer.items) +
      evaluateKnowledge(player = state.hidden.player.revealed, dealer = state.hidden.dealer.belief) +
      evaluateHands(player = state.player.hands, dealer = state.dealer.hands)
