package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.engine.DealerBeliefChecks.missOnShellOut
import com.zkerriga.buckshot.engine.EngineError.*
import com.zkerriga.buckshot.engine.state.PrivateStates.DealerKnowledge
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates}
import com.zkerriga.buckshot.game.events.Shot
import com.zkerriga.buckshot.game.events.outcome.Outcome.{GameOver, Reset}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.partitipant.Side
import com.zkerriga.buckshot.game.state.partitipant.Side.Player
import com.zkerriga.buckshot.game.state.shotgun.Shell
import com.zkerriga.types.Chance

case class PlayerShot(target: Side, shell: Shell)

object PlayerShot:
  def execute(state: GameState, shot: PlayerShot): V[GameOver | Reset | GameState] =
    Shot
      .execute(state.public, Shot(actor = Player, target = shot.target, shell = shot.shell))
      .flatMap:
        case outcome: (GameOver | Reset) => outcome.ok
        case table: TableState =>
          for {
            dealerKnowledge <- updateDealer(old = state.public, table = table, shot, state.hidden.dealer)
          } yield GameState(
            public = table,
            hidden = PrivateStates(
              dealer = dealerKnowledge,
              player = state.hidden.player.afterShellOut,
            ),
          )

  private def updateDealer(
    old: TableState,
    table: TableState,
    shot: PlayerShot,
    knowledge: DealerKnowledge,
  ): V[DealerKnowledge] =
    for {
      adjustedBelief <- knowledge.belief.conditioning { revealed =>
        Chance.certainUnless(missOnShellOut(revealed, old = old.shotgun, updated = table.shotgun, out = shot.shell))
      }
    } yield DealerKnowledge(
      belief = adjustedBelief.update(_.afterShellOut),
      notes = knowledge.notes,
    )
