package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.engine.BeliefState
import com.zkerriga.buckshot.engine.DealerBeliefChecks.missOnShellOut
import com.zkerriga.buckshot.engine.EngineError.*
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.ai.DealerAi.Action
import com.zkerriga.buckshot.engine.state.{GameState, Knowledge, Revealed}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Shot
import com.zkerriga.buckshot.game.events.outcome.Outcome.{GameOver, Reset}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.types.Chance

object DealerShot:
  def execute(state: GameState, shot: Shot[Dealer.type]): V[GameOver | Reset | GameState] =
    Shot
      .execute(state.public, shot)
      .flatMap:
        case outcome: (GameOver | Reset) => outcome.ok
        case table: TableState =>
          for adjustedDealerKnowledge <- state.knowledge.dealer.conditioning(condition(state, table, shot))
          yield GameState(
            public = table,
            knowledge = Knowledge(
              dealer = adjustedDealerKnowledge.update(_.afterShellOut),
              player = state.knowledge.player.afterShellOut,
            ),
          )

  private def condition(oldState: GameState, table: TableState, shot: Shot[Dealer.type])(revealed: Revealed): Chance =
    if missOnShellOut(revealed, old = oldState.shotgun, updated = table.shotgun, out = shot.shell) then Chance.NoChance
    else
      DealerAi.next(oldState.public, revealed) match
        case Action.Use(_, _) => Chance.NoChance
        case Action.Shoot(target) => Chance.certainWhen(target == shot.target)
        case Action.Guess(live, Action.Shoot(Dealer)) =>
          val ifGuessedBlank = Chance.certainWhen(shot.target == Dealer)
          val ifGuessedLive = Chance.certainWhen(live == Action.Shoot(Player) && shot.target == Player)
          (ifGuessedBlank and Chance.CoinFlip) or (ifGuessedLive and Chance.CoinFlip)
