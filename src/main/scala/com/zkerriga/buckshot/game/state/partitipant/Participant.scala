package com.zkerriga.buckshot.game.state.partitipant

import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.*
import com.zkerriga.buckshot.game.state.HealthLimit
import com.zkerriga.buckshot.game.state.items.Item
import com.zkerriga.buckshot.game.state.shotgun.{SeqNr, Shell}

case class Participant(
  health: Health,
  items: Items,
  hands: Hands,
)

object Participant:
  extension (participant: Participant)
    def without(item: Item): V[Participant] =
      participant.items
        .removed(item)
        .map: updated =>
          participant.copy(items = updated)

    def damaged(by: Damage): Option[Participant] =
      participant.health
        .damaged(by)
        .map: updated =>
          participant.copy(health = updated)

    def healed(by: Heal, maxHealth: HealthLimit): Participant =
      participant.copy(health = participant.health.healed(by, maxHealth))

    def cuffed: V[Participant] =
      participant.hands.cuffed.map: updated =>
        participant.copy(hands = updated)

    def afterTurn: Participant =
      participant.copy(hands = participant.hands.afterTurn)

    infix def has(item: Item): Boolean = participant.items.contain(item)
    infix def hasNo(item: Item): Boolean = !has(item)
