package com.zkerriga.buckshot.engine.ai

import com.zkerriga.buckshot.engine.ai.DealerAi.Action
import com.zkerriga.buckshot.engine.state.PrivateStates.DealerNotes
import com.zkerriga.buckshot.engine.state.Revealed
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state.partitipant.{Hands, Health, Items, Participant}
import com.zkerriga.buckshot.game.state.shotgun.Shotgun
import com.zkerriga.types.{Chance, Nat}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DealerAiSpec extends AnyWordSpec, Matchers:
  "DealerAi" should:
    "steal saw" in:
      val state = TableState(
        maxHealth = HealthLimit[3],
        turn = Dealer,
        dealer = Participant(
          health = Health[2],
          items = Items(
            Slot1 -> Adrenaline,
          ),
          hands = Hands.Free,
        ),
        shotgun = Shotgun.fresh(live = Nat[3], blank = Nat[2]),
        player = Participant(
          health = Health[3],
          items = Items(
            Slot1 -> Saw,
            Slot2 -> Meds,
            Slot3 -> Saw,
          ),
          hands = Hands.CuffedForTwoShots,
        ),
      )
      val notes = DealerNotes(usedMeds = false, slotGroups = List(Set.empty))
      DealerAi.next(state, notes, Revealed(Shell1 -> Live)).chanceOf(Action.Steal(ItemOn(Saw, Slot1))) mustNot be(
        Chance.NoChance,
      )
