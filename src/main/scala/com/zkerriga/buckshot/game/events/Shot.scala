package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.events.outcome.Outcome.*
import com.zkerriga.buckshot.game.events.outcome.StateError
import com.zkerriga.buckshot.game.events.outcome.StateError.*
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.partitipant.Side.*
import com.zkerriga.buckshot.game.state.partitipant.{Hands, Participant, Side}
import com.zkerriga.buckshot.game.state.shotgun.Shell.*
import com.zkerriga.buckshot.game.state.shotgun.{Shell, Shotgun}
import com.zkerriga.types.steps.ResultExtension.*
import steps.result.Result

case class Shot(actor: Side, target: Side, shell: Shell)

object Shot:
  private case class PostDamage(dealer: Participant, player: Participant)
  private case class PostShotgun(damaged: PostDamage, shotgun: Shotgun)

  def execute(state: TableState, shot: Shot)(using Raise[StateError]): GameOver | Reset | TableState =
    (state.turn == shot.actor) trueOrRaise WrongTurn
    processDamage(state, shot) match
      case over: GameOver => over
      case damaged: PostDamage =>
        processShotgun(state, shot, damaged) match
          case outcome: (GameOver | Reset) => outcome
          case updated: PostShotgun => buildNextState(state, updated, shot)

  private def processDamage(state: TableState, shot: Shot): GameOver | PostDamage =
    shot.shell match
      case Blank => PostDamage(dealer = state.dealer, player = state.player)
      case Live =>
        shot.target match
          case Player =>
            state.player
              .damaged(state.shotgun.damage)
              .fold(DealerWins): player =>
                PostDamage(dealer = state.dealer, player = player)

          case Dealer =>
            state.dealer
              .damaged(state.shotgun.damage)
              .fold(PlayerWins(dealer = state.dealer.items, player = state.player.items)): dealer =>
                PostDamage(dealer = dealer, player = state.player)

  private def processShotgun(
    state: TableState,
    shot: Shot,
    damaged: PostDamage,
  )(using Raise[StateError]): Reset | PostShotgun =
    state.shotgun.shellOut(shot.shell) match
      case Some(shotgun) => PostShotgun(damaged, shotgun)
      case None =>
        Reset.of(
          maxHealth = state.maxHealth,
          dealer = damaged.dealer,
          player = damaged.player,
        )

  private def buildNextState(state: TableState, updated: PostShotgun, shot: Shot): TableState =
    val keepsTurn = isProlongedTurn(shot)
    val dealer = if keepsTurn then updated.damaged.dealer else updated.damaged.dealer.afterTurn
    val player = if keepsTurn then updated.damaged.player else updated.damaged.player.afterTurn
    val nextTurn = state.turn match
      case Player => if keepsTurn || !dealer.hands.free then Player else Dealer
      case Dealer => if keepsTurn || !player.hands.free then Dealer else Player
    state.copy(
      turn = nextTurn,
      dealer = dealer,
      shotgun = updated.shotgun,
      player = player,
    )

  private def isProlongedTurn(shot: Shot): Boolean = shot.shell == Blank && shot.target == shot.actor
