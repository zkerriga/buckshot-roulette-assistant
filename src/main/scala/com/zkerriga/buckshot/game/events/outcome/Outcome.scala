package com.zkerriga.buckshot.game.events.outcome

import com.zkerriga.buckshot.game.state.HealthLimit
import com.zkerriga.buckshot.game.state.partitipant.{Health, Items, Participant}

object Outcome:
  case object DealerWins

  case class PlayerWins(
    player: Items,
    dealer: Items,
  )

  type GameOver = DealerWins.type | PlayerWins

  case class Reset(
    maxHealth: HealthLimit,
    player: Reset.ParticipantDetails,
    dealer: Reset.ParticipantDetails,
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

    def of(maxHealth: HealthLimit, player: Participant, dealer: Participant): Reset =
      Reset(
        maxHealth = maxHealth,
        player = ParticipantDetails.of(player),
        dealer = ParticipantDetails.of(dealer),
      )
