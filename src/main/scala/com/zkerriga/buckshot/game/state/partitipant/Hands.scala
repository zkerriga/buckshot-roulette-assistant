package com.zkerriga.buckshot.game.state.partitipant

case class Hands(cuffed: Option[Hands.CuffedFor]):
  def free: Boolean = cuffed.isEmpty

object Hands:
  val Free: Hands = Hands(None)
  val Cuffed: Hands = Hands(Some(CuffedFor.TwoShots))

  enum CuffedFor:
    case TwoShots, OneShot

  extension (hands: Hands)
    def postShot: Hands =
      Hands(
        cuffed = hands.cuffed.flatMap:
          case CuffedFor.OneShot => None
          case CuffedFor.TwoShots => Some(CuffedFor.OneShot),
      )
