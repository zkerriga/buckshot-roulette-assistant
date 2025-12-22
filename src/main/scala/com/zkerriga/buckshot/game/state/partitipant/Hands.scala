package com.zkerriga.buckshot.game.state.partitipant

import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.*

case class Hands(state: Option[Hands.CuffedFor]):
  def free: Boolean = state.isEmpty

object Hands:
  val Free: Hands = Hands(None)

  enum CuffedFor:
    case TwoShots, OneShot

  extension (hands: Hands)
    def cuffed: V[Hands] =
      hands.state
        .map(_ => HandsAlreadyCuffed)
        .toLeft(Hands(Some(CuffedFor.TwoShots)))

    def afterShot: Hands =
      Hands(
        state = hands.state.flatMap:
          case CuffedFor.OneShot => None
          case CuffedFor.TwoShots => Some(CuffedFor.OneShot),
      )
