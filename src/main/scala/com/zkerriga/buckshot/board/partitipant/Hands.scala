package com.zkerriga.buckshot.board.partitipant

case class Hands(cuffed: Option[Hands.Cuffed])

object Hands:
  enum Cuffed:
    case ForTwoShots, ForOneShot
