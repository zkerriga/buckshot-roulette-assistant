package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Used.ItemUse
import com.zkerriga.buckshot.game.events.outcome.Outcome.DealerWins
import com.zkerriga.buckshot.game.state.shotgun.Shotgun.{Effects, ShellDistribution}
import com.zkerriga.buckshot.game.state.{HealthLimit, TableState}
import com.zkerriga.types.Nat
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UsedSpec extends AnyWordSpec, Matchers:
  "Used" should:
    "return updated player, dealer, and shotgun after using beer via stealing" in:
      val state = TableState(
        maxHealth = HealthLimit[4],
        turn = Player,
        dealer = Participant(
          health = Health[2],
          items = Items(
            Beer,
            MagnifyingGlass,
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
            Adrenaline,
            Cigarettes,
            Beer,
          ),
          hands = Hands.Free,
        ),
      )
      val used = Used(actor = Player, item = ItemUse.Beer(out = Live), stolen = true)
      Used.execute(state, used) mustBe Right(
        TableState(
          maxHealth = HealthLimit[4],
          turn = Player,
          dealer = Participant(
            health = Health[2],
            items = Items(
              MagnifyingGlass,
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
              Cigarettes,
              Beer,
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
            Meds,
          ),
          hands = Hands.Free,
        ),
      )
      val used = Used(actor = Player, item = ItemUse.Meds(good = false), stolen = false)
      Used.execute(state, used) mustBe Right(DealerWins)
