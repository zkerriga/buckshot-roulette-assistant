package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.*
import com.zkerriga.buckshot.game.events.outcome.Outcome.*
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.partitipant.Side.*
import com.zkerriga.buckshot.game.state.partitipant.{Hands, Participant, Side}
import com.zkerriga.buckshot.game.state.shotgun.Shell.*
import com.zkerriga.buckshot.game.state.shotgun.{Shell, Shotgun}

case class Shot[+Actor <: Side](actor: Actor, target: Side, shell: Shell)

object Shot:
  private case class PostDamage(dealer: Participant, player: Participant)
  private case class PostShotgun(damaged: PostDamage, shotgun: Shotgun)

  def execute(state: TableState, shot: Shot[Side]): V[GameOver | Reset | TableState] =
    for
      _ <- (state.turn == shot.actor) trueOr WrongTurn
      postShotgun <- processDamage(state, shot) match
        case result: GameOver => result.ok
        case damaged: PostDamage => processShotgun(state, shot, damaged)
    yield postShotgun match
      case outcome: (GameOver | Reset) => outcome
      case updated: PostShotgun => buildNextState(state, updated, shot)

  private def processDamage(state: TableState, shot: Shot[Side]): GameOver | PostDamage =
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

  private def processShotgun(state: TableState, shot: Shot[Side], damaged: PostDamage): V[Reset | PostShotgun] =
    state.shotgun
      .shellOut(shot.shell)
      .map:
        case Some(shotgun) => PostShotgun(damaged, shotgun)
        case None =>
          Reset.of(
            maxHealth = state.maxHealth,
            dealer = damaged.dealer,
            player = damaged.player,
          )

  private def buildNextState(state: TableState, updated: PostShotgun, shot: Shot[Side]): TableState =
    val dealer = updated.damaged.dealer.afterShot
    val player = updated.damaged.player.afterShot
    val nextTurn = state.turn match
      case Player => if keepsTurn(shot, opponent = dealer) then Player else Dealer
      case Dealer => if keepsTurn(shot, opponent = player) then Dealer else Player
    state.copy(
      turn = nextTurn,
      dealer = dealer,
      shotgun = updated.shotgun,
      player = player,
    )

  private def keepsTurn(shot: Shot[Side], opponent: Participant): Boolean =
    (shot.shell == Blank && shot.target == shot.actor) || !opponent.hands.free
