package com.zkerriga.buckshot.engine.events

import cats.Eq
import com.zkerriga.buckshot.engine.DealerBeliefChecks.missOnShellOut
import com.zkerriga.buckshot.engine.EngineError.*
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerKnowledge, DealerNotes}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Shot
import com.zkerriga.buckshot.game.events.outcome.Outcome.{GameOver, Reset}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.types.Chance

case class DealerShot(target: Side, shell: Shell)

object DealerShot:
  def execute(state: GameState, shot: DealerShot): V[GameOver | Reset | GameState] =
    Shot
      .execute(state.public, Shot(actor = Dealer, target = shot.target, shell = shot.shell))
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
    shot: DealerShot,
    knowledge: DealerKnowledge,
  ): V[DealerKnowledge] =
    for {
      adjustedBelief <- knowledge.belief.conditioning(
        condition(old = old, oldNotes = knowledge.notes, table = table, shot),
      )
    } yield DealerKnowledge(
      belief = adjustedBelief.update(_.afterShellOut),
      notes = knowledge.notes.copy(usedMeds = false),
    )

  private def condition(old: TableState, oldNotes: DealerNotes, table: TableState, shot: DealerShot)(
    revealed: Revealed,
  ): Chance =
    if missOnShellOut(revealed, old = old.shotgun, updated = table.shotgun, out = shot.shell) then Chance.NoChance
    else {
      val realAction = DealerAi.Action.Shoot(shot.target)
      val prediction = DealerAi.next(old, oldNotes, revealed)
      prediction.chanceOf(realAction)(using Eq.fromUniversalEquals) // todo: fix Eq
    }
