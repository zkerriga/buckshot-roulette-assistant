package com.zkerriga.buckshot.engine

import com.zkerriga.buckshot.engine.events.DealerUsed
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerKnowledge, DealerNotes, PlayerKnowledge}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.ItemUse
import com.zkerriga.buckshot.game.state.shotgun.Shotgun.{Effects, ShellDistribution}
import com.zkerriga.types.Nat
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import steps.result.Result

class EngineSpec extends AnyWordSpec, Matchers:
  "Engine" should:
    "process using Magnifying Glass" in:
      val table = TableState(
        maxHealth = HealthLimit[2],
        turn = Dealer,
        dealer = Participant(
          health = Health[1],
          items = Items(
            adrenaline = Set(),
            positioned = List(
              ItemOn(item = Inverter, on = Slot3),
              ItemOn(item = Meds, on = Slot4),
              ItemOn(item = MagnifyingGlass, on = Slot6),
              ItemOn(item = Beer, on = Slot7),
              ItemOn(item = Inverter, on = Slot8),
            ),
          ),
          hands = Hands.Free,
        ),
        shotgun = Shotgun(
          shells = ShellDistribution(live = Nat[3], Nat[3]),
          effects = Effects(damage = Damage.Single, inverted = false),
        ),
        player = Participant(
          health = Health[1],
          items = Items(adrenaline = Set(), positioned = List(ItemOn(item = Saw, on = Slot2))),
          hands = Hands.Free,
        ),
      )

      val hidden = PrivateStates(
        DealerKnowledge(
          BeliefState.deterministic(Revealed.Nothing),
          DealerNotes(false, List(Set(Slot4, Slot3, Slot8), Set(Slot6, Slot7))),
        ),
        PlayerKnowledge(Revealed(Shell5 -> Blank)),
      )

      val engine = Engine.start(GameState(table, hidden))
      engine.process(DealerUsed(ItemUse.MagnifyingGlass, Slot6, None)) match {
        case Result.Err(error) => fail(s"unexpected $error")
        case Result.Ok(_) => succeed
      }
