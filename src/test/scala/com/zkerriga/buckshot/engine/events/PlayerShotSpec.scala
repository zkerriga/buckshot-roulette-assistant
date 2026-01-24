package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.engine.BeliefState
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerKnowledge, DealerNotes, PlayerKnowledge}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state.shotgun.Shotgun.{Effects, ShellDistribution}
import com.zkerriga.types.{Chance, Nat}
import com.zkerriga.types.steps.ResultExtension.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import steps.result.Result

class PlayerShotSpec extends AnyWordSpec, Matchers:
  "execute" should:
    "update dealer knowledge belief" in:
      val state = GameState(
        public = TableState(
          maxHealth = HealthLimit[4],
          turn = Player,
          dealer = Participant(
            health = Health[4],
            items = Items(),
            hands = Hands.Free,
          ),
          shotgun = Shotgun(
            shells = ShellDistribution(live = Nat[3], blank = Nat[3]),
            effects = Effects.Default,
          ),
          player = Participant(
            health = Health[4],
            items = Items(),
            hands = Hands.Free,
          ),
        ),
        hidden = PrivateStates(
          dealer = DealerKnowledge(
            BeliefState.unsafe(
              /** preserved, because dealer had correct assumption about the first shell
                */
              (Chance.CoinFlip and Chance.CoinFlip) -> Revealed(Shell1 -> Live, Shell3 -> Blank),

              /** eliminated, because dealer assumption about the first shell was wrong
                */
              (Chance.CoinFlip and Chance.CoinFlip) -> Revealed(Shell1 -> Blank, Shell3 -> Blank),

              /** preserved, because after update, dealer knowledge doesn't conflict with shell distribution
                */
              (Chance.CoinFlip and Chance.CoinFlip) -> Revealed(
                Shell2 -> Live,
                Shell3 -> Live,
                Shell4 -> Blank,
                Shell5 -> Blank,
                Shell6 -> Blank,
              ),
              /** eliminated, because after update, dealer knowledge conflicts with shell distribution
                */
              (Chance.CoinFlip and Chance.CoinFlip) -> Revealed(
                Shell2 -> Live,
                Shell3 -> Live,
                Shell4 -> Live,
                Shell5 -> Blank,
                Shell6 -> Blank,
              ),
            ),
            notes = DealerNotes(
              usedMeds = false,
              slotGroups = List(),
            ),
          ),
          player = PlayerKnowledge(Revealed(Shell2 -> Live)),
        ),
      )

      val event = PlayerShot(target = Dealer, shell = Live)
      Result.scope(PlayerShot.execute(state, event)) match
        case Result.Ok(state: GameState) =>
          state.shotgun.shells mustBe ShellDistribution(live = Nat[2], blank = Nat[3])
          state.hidden mustBe PrivateStates(
            dealer = DealerKnowledge(
              BeliefState.unsafe(
                Chance.CoinFlip -> Revealed(Shell2 -> Blank),
                Chance.CoinFlip -> Revealed(
                  Shell1 -> Live,
                  Shell2 -> Live,
                  Shell3 -> Blank,
                  Shell4 -> Blank,
                  Shell5 -> Blank,
                ),
              ),
              DealerNotes(
                usedMeds = false,
                slotGroups = List(),
              ),
            ),
            player = PlayerKnowledge(Revealed(Shell1 -> Live)),
          )
        case other => fail(s"unexpected: $other")
