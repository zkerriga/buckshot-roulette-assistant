package com.zkerriga.buckshot.game.state.partitipant

import com.zkerriga.buckshot.game.state.HealthLimit
import com.zkerriga.buckshot.game.state.items.Item
import com.zkerriga.buckshot.game.state.shotgun.{SeqNr, Shell}

case class Participant(
  health: Health,
  items: Items,
  hands: Hands,
  revealed: Revealed,
)

object Participant:
  extension (participant: Participant)
    def without(item: Item): Option[Participant] =
      participant.items
        .removed(item)
        .map: updated =>
          participant.copy(items = updated)

    def knowing(shell: Shell, at: SeqNr): Participant =
      participant.copy(revealed = participant.revealed.revealed(shell, at))

    def damaged(by: Damage): Option[Participant] =
      participant.health
        .damaged(by)
        .map: updated =>
          participant.copy(health = updated)

    def healed(by: Heal, maxHealth: HealthLimit): Participant =
      participant.copy(health = participant.health.healed(by, maxHealth))

    def cuffed: Participant =
      participant.copy(hands = Hands.Cuffed)

    def postShot: Participant =
      participant.copy(
        hands = participant.hands.postShot,
        revealed = participant.revealed.postShot,
      )
