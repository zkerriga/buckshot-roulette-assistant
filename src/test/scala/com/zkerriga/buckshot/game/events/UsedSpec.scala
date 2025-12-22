package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.events.Used.ItemUse
import com.zkerriga.buckshot.game.events.outcome.Outcome.DealerWins
import com.zkerriga.buckshot.game.state.items.*
import com.zkerriga.buckshot.game.state.partitipant.*
import com.zkerriga.buckshot.game.state.partitipant.Side.Player
import com.zkerriga.buckshot.game.state.shotgun.SeqNr.*
import com.zkerriga.buckshot.game.state.shotgun.Shell.*
import com.zkerriga.buckshot.game.state.shotgun.Shotgun
import com.zkerriga.buckshot.game.state.shotgun.Shotgun.{Effects, ShellDistribution}
import com.zkerriga.buckshot.game.state.{GameState, HealthLimit}
import com.zkerriga.types.Nat
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UsedSpec extends AnyWordSpec, Matchers:
  "Used" should:
    "return updated player, dealer, and shotgun after using beer via stealing" in:
      val state = GameState(
        maxHealth = HealthLimit[4],
        player = Participant(
          health = Health[2],
          items = Items(
            Adrenaline,
            Cigarettes,
            Beer,
          ),
          hands = Hands.Free,
          revealed = Revealed(Shell3 -> Live),
        ),
        dealer = Participant(
          health = Health[2],
          items = Items(
            Beer,
            MagnifyingGlass,
          ),
          hands = Hands.CuffedForOneShot,
          revealed = Revealed(Shell2 -> Blank),
        ),
        shotgun = Shotgun(
          shells = ShellDistribution(live = Nat[3], blank = Nat[3]),
          effects = Effects(
            damage = Damage.Single,
            inverted = true,
          ),
        ),
        turnOf = Player,
      )
      val used = Used(actor = Player, item = ItemUse.Beer(out = Live), stolen = true)
      Used.execute(state, used) mustBe Right(
        GameState(
          maxHealth = HealthLimit[4],
          player = Participant(
            health = Health[2],
            items = Items(
              Cigarettes,
              Beer,
            ),
            hands = Hands.Free,
            revealed = Revealed(Shell2 -> Live),
          ),
          dealer = Participant(
            health = Health[2],
            items = Items(
              MagnifyingGlass,
            ),
            hands = Hands.CuffedForOneShot,
            revealed = Revealed(Shell1 -> Blank),
          ),
          shotgun = Shotgun(
            shells = ShellDistribution(live = Nat[3], blank = Nat[2]),
            effects = Effects(
              damage = Damage.Single,
              inverted = false,
            ),
          ),
          turnOf = Player,
        ),
      )

    "return GameOver after using bad Meds" in:
      val state = GameState(
        maxHealth = HealthLimit[4],
        player = Participant(
          health = Health[1],
          items = Items(
            Meds,
          ),
          hands = Hands.Free,
          revealed = Revealed(),
        ),
        dealer = Participant(
          health = Health[2],
          items = Items(),
          hands = Hands.Free,
          revealed = Revealed(),
        ),
        shotgun = Shotgun(
          shells = ShellDistribution(live = Nat[3], blank = Nat[3]),
          effects = Effects.Default,
        ),
        turnOf = Player,
      )
      val used = Used(actor = Player, item = ItemUse.Meds(good = false), stolen = false)
      Used.execute(state, used) mustBe Right(DealerWins)
