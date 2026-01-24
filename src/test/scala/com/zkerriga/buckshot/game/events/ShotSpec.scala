package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.outcome.Outcome.{DealerWins, PlayerWins}
import com.zkerriga.buckshot.game.state.shotgun.Shotgun.{Effects, ShellDistribution}
import com.zkerriga.buckshot.game.state.{HealthLimit, TableState}
import com.zkerriga.types.steps.ResultExtension.*
import com.zkerriga.types.{Nat, Quantity}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import steps.result.Result

class ShotSpec extends AnyWordSpec, Matchers:
  "Shot" should:
    "return GameOver if player is killed by dealer's shot" in:
      val state = TableState(
        maxHealth = HealthLimit[4],
        turn = Dealer,
        dealer = Participant(
          health = Health[2],
          items = Items(),
          hands = Hands.Free,
        ),
        shotgun = Shotgun(
          shells = ShellDistribution(live = Nat[1], blank = Nat[0]),
          effects = Effects.Default,
        ),
        player = Participant(
          health = Health[1],
          items = Items(),
          hands = Hands.Free,
        ),
      )
      val shot = Shot(actor = Dealer, target = Player, shell = Live)
      Result.scope(Shot.execute(state, shot)) mustBe Result.Ok(DealerWins)

    "return GameOver if dealer is killed by player's shot with saw" in:
      val state = TableState(
        maxHealth = HealthLimit[4],
        turn = Player,
        dealer = Participant(
          health = Health[2],
          items = Items(
            Slot1 -> Beer,
          ),
          hands = Hands.Free,
        ),
        shotgun = Shotgun(
          shells = ShellDistribution(live = Nat[1], blank = Nat[0]),
          effects = Effects(
            damage = Damage.Double,
            inverted = false,
          ),
        ),
        player = Participant(
          health = Health[2],
          items = Items(
            Slot1 -> Cigarettes,
            Slot2 -> MagnifyingGlass,
          ),
          hands = Hands.Free,
        ),
      )
      val shot = Shot(actor = Player, target = Dealer, shell = Live)
      Result.scope(Shot.execute(state, shot)) mustBe Result.Ok(
        PlayerWins(player = Items(Slot1 -> Cigarettes, Slot2 -> MagnifyingGlass), dealer = Items(Slot1 -> Beer)),
      )
