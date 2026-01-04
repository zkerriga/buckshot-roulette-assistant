package com.zkerriga.buckshot.engine.state

import com.zkerriga.buckshot.engine.BeliefState
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerKnowledge, PlayerKnowledge}
import com.zkerriga.buckshot.game.state.items.{RegularItem, Slot}
import com.zkerriga.buckshot.game.state.shotgun.{SeqNr, Shell}

case class PrivateStates(
  dealer: DealerKnowledge,
  player: PlayerKnowledge,
)

object PrivateStates:
  case class PlayerKnowledge(revealed: Revealed)
  object PlayerKnowledge:
    extension (knowledge: PlayerKnowledge)
      def knowing(shell: Shell, at: SeqNr): PlayerKnowledge =
        knowledge.copy(revealed = knowledge.revealed.revealed(shell, at))
      def afterShellOut: PlayerKnowledge =
        knowledge.copy(revealed = knowledge.revealed.afterShellOut)

  /** @param usedMeds
    *   dealer doesn't use [[com.zkerriga.buckshot.game.state.items.RegularItem.Meds]] twice during one turn (until any
    *   shot is performed), so I need to keep that info between independent actions
    * @param slotGroups
    *   keeps track of which set of items came first to the dealer. This is helpful, because dealer uses items based on
    *   their order. We don't know the order of specific items in a round, but we know that items from a previous round
    *   are going to be checked earlier than items that came later
    */
  case class DealerNotes(
    usedMeds: Boolean,
    slotGroups: List[Set[Slot]],
  )
  object DealerNotes:
    extension (notes: DealerNotes)
      def usingMeds: DealerNotes =
        notes.copy(usedMeds = true)
      def afterDealerShot: DealerNotes =
        notes.copy(usedMeds = false)
      def withoutItemOn(slot: Slot): DealerNotes =
        notes.copy(slotGroups = notes.slotGroups.map(_ - slot).filter(_.nonEmpty))

  /** @param slotSets
    *   keeps track of which set of items came first to the dealer. This is helpful, because dealer uses items based on
    *   their order. We don't know the order of specific items in a round, but we know that items from a previous round
    *   are going to be checked earlier than items that came later
    */
  case class DealerKnowledge(
    belief: BeliefState[Revealed],
    notes: DealerNotes,
  )
