package com.zkerriga.buckshot.game.state.partitipant

case class Hands(cuffed: Option[Hands.Cuffed])

object Hands:
  enum Cuffed:
    case ForTwoShots, ForOneShot
