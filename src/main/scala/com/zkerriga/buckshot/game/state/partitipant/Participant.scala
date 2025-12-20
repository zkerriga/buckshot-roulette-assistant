package com.zkerriga.buckshot.game.state.partitipant

case class Participant(
  health: Health,
  items: Items,
  hands: Hands,
  revealed: Revealed,
)

object Participant:
  extension (participant: Participant)
    def damaged(by: Damage): Option[Participant] =
      participant.health
        .damaged(by)
        .map: updated =>
          participant.copy(health = updated)

    def postShot: Participant =
      participant.copy(
        hands = participant.hands.postShot,
        revealed = participant.revealed.postShot,
      )
