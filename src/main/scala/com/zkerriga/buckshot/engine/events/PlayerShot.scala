package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.engine.BeliefState
import com.zkerriga.buckshot.engine.DealerBeliefChecks.missOnShellOut
import com.zkerriga.buckshot.engine.EngineError.*
import com.zkerriga.buckshot.engine.state.{GameState, Knowledge}
import com.zkerriga.buckshot.game.events.Shot
import com.zkerriga.buckshot.game.events.outcome.Outcome.{GameOver, Reset}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.partitipant.Side.Player
import com.zkerriga.types.Chance

object PlayerShot:
  def execute(state: GameState, shot: Shot[Player.type]): V[GameOver | Reset | GameState] =
    Shot
      .execute(state.public, shot)
      .flatMap:
        case outcome: (GameOver | Reset) => outcome.ok
        case table: TableState =>
          for adjustedDealerKnowledge <- state.knowledge.dealer.conditioning: revealed =>
              Chance.certainUnless:
                missOnShellOut(revealed, old = state.shotgun, updated = table.shotgun, out = shot.shell)
          yield GameState(
            public = table,
            knowledge = Knowledge(
              dealer = adjustedDealerKnowledge.update(_.afterShellOut),
              player = state.knowledge.player.afterShellOut,
            ),
          )
