package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.*
import com.zkerriga.buckshot.game.events.outcome.Outcome.*
import com.zkerriga.buckshot.game.state.GameState
import com.zkerriga.buckshot.game.state.partitipant.Side.*
import com.zkerriga.buckshot.game.state.partitipant.{Participant, Side}
import com.zkerriga.buckshot.game.state.shotgun.Shell.*
import com.zkerriga.buckshot.game.state.shotgun.{Shell, Shotgun}

case class Shot(actor: Side, target: Side, shell: Shell)

object Shot:
  private case class PostDamage(player: Participant, dealer: Participant)
  private case class PostShotgun(damaged: PostDamage, shotgun: Shotgun)

  def execute(state: GameState, shot: Shot): V[GameOver | Reset | GameState] =
    for
      _ <- (state.turnOf == shot.actor) trueOr WrongTurn
      postShotgun <- processDamage(state, shot) match
        case result: GameOver => result.ok
        case damaged: PostDamage => processShotgun(state, shot, damaged)
    yield postShotgun match
      case outcome: (GameOver | Reset) => outcome
      case updated: PostShotgun => buildNextState(state, updated, shot)

  private def processDamage(state: GameState, shot: Shot): GameOver | PostDamage =
    shot.shell match
      case Blank => PostDamage(player = state.player, dealer = state.dealer)
      case Live =>
        shot.target match
          case Player =>
            state.player
              .damaged(state.shotgun.damage)
              .fold(DealerWins): player =>
                PostDamage(player = player, dealer = state.dealer)

          case Dealer =>
            state.dealer
              .damaged(state.shotgun.damage)
              .fold(PlayerWins(player = state.player.items, dealer = state.dealer.items)): dealer =>
                PostDamage(player = state.player, dealer = dealer)

  private def processShotgun(state: GameState, shot: Shot, damaged: PostDamage): V[Reset | PostShotgun] =
    state.shotgun
      .shellOut(shot.shell)
      .map:
        case Some(shotgun) => PostShotgun(damaged, shotgun)
        case None =>
          Reset.of(
            maxHealth = state.maxHealth,
            player = damaged.player,
            dealer = damaged.dealer,
          )

  private def buildNextState(state: GameState, updated: PostShotgun, shot: Shot): GameState =
    val player = updated.damaged.player.postShot
    val dealer = updated.damaged.dealer.postShot
    val nextTurnOf = state.turnOf match
      case Player => if keepsTurn(shot, opponent = dealer) then Player else Dealer
      case Dealer => if keepsTurn(shot, opponent = player) then Dealer else Player
    state.copy(
      shotgun = updated.shotgun,
      player = player,
      dealer = dealer,
      turnOf = nextTurnOf,
    )

  private def keepsTurn(shot: Shot, opponent: Participant): Boolean =
    (shot.shell == Blank && shot.target == shot.actor) || !opponent.hands.free
