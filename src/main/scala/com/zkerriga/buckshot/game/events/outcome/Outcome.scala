package com.zkerriga.buckshot.game.events.outcome

import com.zkerriga.buckshot.game.state.HealthLimit
import com.zkerriga.buckshot.game.state.partitipant.{Health, Items}

object Outcome:
  case object DealerWins

  case class PlayerWins(
    player: Items,
    dealer: Items,
  )

  type GameOver = DealerWins.type | PlayerWins

  case class Reset(
    maxHealth: HealthLimit,
    player: Reset.Participant,
    dealer: Reset.Participant,
  )
  object Reset:
    case class Participant(
      health: Health,
      items: Items,
    )
