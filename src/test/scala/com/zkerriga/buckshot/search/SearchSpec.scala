package com.zkerriga.buckshot.search

import com.zkerriga.buckshot.engine.BeliefState
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerKnowledge, DealerNotes, PlayerKnowledge}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state.shotgun.Shotgun.{Effects, ShellDistribution}
import com.zkerriga.types.Nat
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import steps.result.Result

class SearchSpec extends AnyWordSpec, Matchers:
  "best" should:
    "find the simplest path" in:
      val state: GameState = GameState(
        TableState(
          maxHealth = HealthLimit[3],
          turn = Player,
          dealer = Participant(
            health = Health[1],
            items = Items(
              adrenaline = Set(Slot6),
              positioned = List(
                ItemOn(item = Beer, on = Slot1),
                ItemOn(item = BurnerPhone, on = Slot2),
                ItemOn(item = Saw, on = Slot3),
                ItemOn(item = MagnifyingGlass, on = Slot4),
                ItemOn(item = Beer, on = Slot5),
                ItemOn(item = Inverter, on = Slot8),
              ),
            ),
            hands = Hands.Free,
          ),
          shotgun = Shotgun(
            shells = ShellDistribution(live = Nat[1], Nat[0]),
            effects = Effects.Default,
          ),
          player = Participant(
            health = Health[2],
            items = Items(
              adrenaline = Set(Slot1),
              positioned = List(
                ItemOn(item = Saw, on = Slot2),
                ItemOn(item = MagnifyingGlass, on = Slot3),
                ItemOn(item = BurnerPhone, on = Slot4),
              ),
            ),
            hands = Hands.Free,
          ),
        ),
        PrivateStates(
          DealerKnowledge(
            BeliefState.deterministic(Revealed.Nothing),
            DealerNotes(false, List(Set(Slot5, Slot1, Slot4, Slot3, Slot8, Slot2))),
          ),
          PlayerKnowledge(Revealed.Nothing),
        ),
      )

      Search.best(state).map(_.action) shouldBe Result.Ok(Search.Action.Shoot(Dealer))
