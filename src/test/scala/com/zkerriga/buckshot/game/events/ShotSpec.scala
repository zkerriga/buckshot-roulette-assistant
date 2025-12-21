package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.events.outcome.Outcome.{DealerWins, PlayerWins}
import com.zkerriga.buckshot.game.state.items.*
import com.zkerriga.buckshot.game.state.partitipant.*
import com.zkerriga.buckshot.game.state.partitipant.Side.{Dealer, Player}
import com.zkerriga.buckshot.game.state.shotgun.Shell.*
import com.zkerriga.buckshot.game.state.shotgun.Shotgun
import com.zkerriga.buckshot.game.state.shotgun.Shotgun.{Effects, ShellDistribution}
import com.zkerriga.buckshot.game.state.{GameState, HealthLimit}
import com.zkerriga.types.{Nat, Quantity}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ShotSpec extends AnyWordSpec, Matchers:
  "Shot" should {
    "return GameOver if player is killed by dealer's shot" in {
      val state = GameState(
        maxHealth = HealthLimit[4],
        player = Participant(
          health = Health[1],
          items = Items(),
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
          shells = ShellDistribution(live = Nat[1], blank = Nat[0]),
          effects = Effects.Default,
        ),
        turnOf = Dealer,
      )
      val shot = Shot(actor = Dealer, target = Player, shell = Live)

      Shot.execute(state, shot) mustBe Right(DealerWins)
    }

    "return GameOver if dealer is killed by player's shot with saw" in {
      val state = GameState(
        maxHealth = HealthLimit[4],
        player = Participant(
          health = Health[2],
          items = Items(
            Cigarettes,
            MagnifyingGlass,
          ),
          hands = Hands.Free,
          revealed = Revealed(),
        ),
        dealer = Participant(
          health = Health[2],
          items = Items(
            Beer,
          ),
          hands = Hands.Free,
          revealed = Revealed(),
        ),
        shotgun = Shotgun(
          shells = ShellDistribution(live = Nat[1], blank = Nat[0]),
          effects = Effects(
            damage = Damage.Double,
            inverted = false,
          ),
        ),
        turnOf = Player,
      )
      val shot = Shot(actor = Player, target = Dealer, shell = Live)

      Shot.execute(state, shot) mustBe Right(
        PlayerWins(player = Items(Cigarettes, MagnifyingGlass), dealer = Items(Beer)),
      )
    }
  }
