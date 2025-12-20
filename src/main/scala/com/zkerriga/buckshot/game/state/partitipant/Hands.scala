package com.zkerriga.buckshot.game.state.partitipant

case class Hands(cuffed: Option[Hands.Cuffed]):
  def free: Boolean = cuffed.isEmpty

object Hands:
  val Free: Hands = Hands(None)

  enum Cuffed:
    case ForTwoShots, ForOneShot

  extension (hands: Hands)
    def postShot: Hands =
      Hands(
        cuffed = hands.cuffed.flatMap:
          case Hands.Cuffed.ForOneShot => None
          case Hands.Cuffed.ForTwoShots => Some(Hands.Cuffed.ForOneShot),
      )
