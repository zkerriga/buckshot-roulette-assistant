package com.zkerriga.buckshot.engine.ai

import com.zkerriga.buckshot.engine.ai.DealerAi.Action
import com.zkerriga.buckshot.engine.state.Revealed
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state.partitipant.Damage.Single
import com.zkerriga.buckshot.game.state.partitipant.Hands.Free
import com.zkerriga.buckshot.game.state.partitipant.{Hands, Health, Items, Participant}
import com.zkerriga.buckshot.game.state.shotgun.Shotgun
import com.zkerriga.buckshot.game.state.shotgun.Shotgun.{Effects, ShellDistribution}
import com.zkerriga.types.Nat
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
            Adrenaline,
            Adrenaline,
            Inverter,
            Meds,
          ),
          hands = Hands.Free,
        ),
        shotgun = Shotgun.fresh(live = Nat[3], blank = Nat[2]),
        player = Participant(
          health = Health[3],
          items = Items(
            Meds,
            Saw,
            Saw,
          ),
          hands = Hands.CuffedForTwoShots,
        ),
      )
      // dealer stole Saw in real game
      DealerAi.next(state, Revealed()) mustBe Action.Use(Saw, steal = true)

    "use beer" in:
      val state = TableState(
        maxHealth = HealthLimit[4],
        turn = Dealer,
        dealer = Participant(health = Health[3], items = Items(Beer, Cigarettes), hands = Free),
        shotgun = Shotgun(
          shells = ShellDistribution(live = Nat[0], Nat[2]),
          effects = Effects(damage = Single, inverted = false),
        ),
        player = Participant(health = Health[4], items = Items(Inverter), hands = Free),
      )
      // Used(Dealer,Beer(Blank),false)
      DealerAi.next(state, Revealed()) mustBe Action.Use(Beer, steal = false)

    "used good meds" in:
      val state =
        TableState(
          maxHealth = HealthLimit[4],
          turn = Dealer,
          dealer = Participant(
            health = Health[3],
            items = Items(MagnifyingGlass, MagnifyingGlass, Saw, Adrenaline, Adrenaline, Meds, Inverter),
            hands = Hands.Free,
          ),
          shotgun = Shotgun(
            shells = ShellDistribution(live = Nat[1], Nat[1]),
            effects = Effects(damage = Damage.Single, inverted = false),
          ),
          player = Participant(health = Health[4], items = Items(Saw, Meds), hands = Hands.Free),
        )
      // Used(Dealer,Meds(true),false)
      DealerAi.next(state, Revealed()) mustBe Action.Use(Meds, steal = false)

    "used burner phone" in:
      val state =
        TableState(
          maxHealth = HealthLimit[4],
          turn = Dealer,
          dealer = Participant(health = Health[4], items = Items(BurnerPhone, Beer), hands = Hands.Free),
          shotgun = Shotgun(
            shells = ShellDistribution(live = Nat[3], Nat[4]),
            effects = Effects(damage = Damage.Single, inverted = false),
          ),
          player = Participant(health = Health[3], items = Items(Beer), hands = Hands.Free),
        )
      // Used(Dealer,BurnerPhone,false)
      DealerAi.next(state, Revealed()) mustBe Action.Use(BurnerPhone, steal = false)

    // Used(Dealer,MagnifyingGlass,false)
    "used magnifying glass" in:
      val state =
        TableState(
          maxHealth = HealthLimit[4],
          turn = Dealer,
          dealer = Participant(
            health = Health[1],
            items = Items(Cigarettes, Saw, MagnifyingGlass, Handcuffs),
            hands = Hands.Free,
          ),
          shotgun = Shotgun(
            shells = ShellDistribution(live = Nat[1], Nat[2]),
            effects = Effects(damage = Damage.Single, inverted = false),
          ),
          player = Participant(health = Health[2], items = Items(Meds), hands = Hands.Free),
        )
      DealerAi.next(state, Revealed()) mustBe Action.Use(MagnifyingGlass, steal = false)
