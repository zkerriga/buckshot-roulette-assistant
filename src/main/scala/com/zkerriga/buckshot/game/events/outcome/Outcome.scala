package com.zkerriga.buckshot.game.events.outcome

import com.zkerriga.buckshot.game.state.HealthLimit
import com.zkerriga.buckshot.game.state.partitipant.{Health, Items, Participant}

object Outcome:
  case object DealerWins

  case class PlayerWins(
    dealer: Items,
    player: Items,
  )

  type GameOver = DealerWins.type | PlayerWins

  case class Reset(
    maxHealth: HealthLimit,
    dealer: Reset.ParticipantDetails,
    player: Reset.ParticipantDetails,
  )
  object Reset:
    case class ParticipantDetails(
      health: Health,
      items: Items,
    )
    object ParticipantDetails:
      def of(participant: Participant): ParticipantDetails =
        ParticipantDetails(
          health = participant.health,
          items = participant.items,
        )

    def of(maxHealth: HealthLimit, dealer: Participant, player: Participant): Reset =
      Reset(
        maxHealth = maxHealth,
        dealer = ParticipantDetails.of(dealer),
        player = ParticipantDetails.of(player),
      )
