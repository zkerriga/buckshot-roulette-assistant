package com.zkerriga.buckshot.game.state.partitipant

import com.zkerriga.buckshot.game.events.outcome.StateError.*
import com.zkerriga.buckshot.game.state.HealthLimit
import com.zkerriga.buckshot.game.state.items.{RegularItem, Slot}
import com.zkerriga.buckshot.game.state.partitipant.Items.ItemOn
import com.zkerriga.types.steps.ResultExtension.*

case class Participant(
  health: Health,
  items: Items,
  hands: Hands,
)

object Participant:
  extension (participant: Participant)
    def without(item: ItemOn): Participant =
      participant.copy(items = participant.items.without(item))

    def withoutAdrenaline(on: Slot): Participant =
      participant.copy(items = participant.items.withoutAdrenaline(on))

    def has(item: RegularItem): Boolean =
      participant.items.contain(item)
    def hasAdrenaline: Boolean =
      participant.items.containAdrenaline

    def damaged(by: Damage): Option[Participant] =
      participant.health
        .damaged(by)
        .map: updated =>
          participant.copy(health = updated)

    def healed(by: Heal, maxHealth: HealthLimit): Participant =
      participant.copy(health = participant.health.healed(by, maxHealth))

    def cuffed(using Raise[HandsAlreadyCuffed.type]): Participant =
      participant.copy(hands = participant.hands.cuffed)

    def afterTurn: Participant =
      participant.copy(hands = participant.hands.afterTurn)
