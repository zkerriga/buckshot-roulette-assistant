package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.ItemUse
import com.zkerriga.buckshot.game.events.outcome.Outcome.DealerWins
import com.zkerriga.buckshot.game.state.shotgun.Shotgun.{Effects, ShellDistribution}
import com.zkerriga.buckshot.game.state.{HealthLimit, TableState}
import com.zkerriga.types.Nat
import com.zkerriga.types.steps.ResultExtension.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import steps.result.Result

class UsedSpec extends AnyWordSpec, Matchers:
  "Used" should:
    "return updated player, dealer, and shotgun after using beer via stealing" in:
      val state = TableState(
        maxHealth = HealthLimit[4],
        turn = Player,
        dealer = Participant(
          health = Health[2],
          items = Items(
            Slot1 -> Beer,
            Slot2 -> MagnifyingGlass,
          ),
          hands = Hands.CuffedForOneShot,
        ),
        shotgun = Shotgun(
          shells = ShellDistribution(live = Nat[3], blank = Nat[3]),
          effects = Effects(
            damage = Damage.Single,
            inverted = true,
          ),
        ),
        player = Participant(
          health = Health[2],
          items = Items(
            Slot1 -> Adrenaline,
            Slot2 -> Cigarettes,
            Slot3 -> Beer,
          ),
          hands = Hands.Free,
        ),
      )
      val used = Used(actor = Player, item = ItemUse.Beer(out = Live), on = Slot1, viaAdrenalineOn = Some(Slot1))
      Result.scope(Used.execute(state, used)) mustBe Result.Ok(
        TableState(
          maxHealth = HealthLimit[4],
          turn = Player,
          dealer = Participant(
            health = Health[2],
            items = Items(
              Slot2 -> MagnifyingGlass,
            ),
            hands = Hands.CuffedForOneShot,
          ),
          shotgun = Shotgun(
            shells = ShellDistribution(live = Nat[3], blank = Nat[2]),
            effects = Effects(
              damage = Damage.Single,
              inverted = false,
            ),
          ),
          player = Participant(
            health = Health[2],
            items = Items(
              Slot2 -> Cigarettes,
              Slot3 -> Beer,
            ),
            hands = Hands.Free,
          ),
        ),
      )

    "return GameOver after using bad Meds" in:
      val state = TableState(
        maxHealth = HealthLimit[4],
        turn = Player,
        dealer = Participant(
          health = Health[2],
          items = Items(),
          hands = Hands.Free,
        ),
        shotgun = Shotgun(
          shells = ShellDistribution(live = Nat[3], blank = Nat[3]),
          effects = Effects.Default,
        ),
        player = Participant(
          health = Health[1],
          items = Items(
            Slot1 -> Meds,
          ),
          hands = Hands.Free,
        ),
      )
      val used = Used(actor = Player, item = ItemUse.Meds(good = false), on = Slot1, viaAdrenalineOn = None)
      Result.scope(Used.execute(state, used)) mustBe Result.Ok(DealerWins)
