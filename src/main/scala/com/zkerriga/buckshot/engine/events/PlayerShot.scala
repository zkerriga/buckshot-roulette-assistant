package com.zkerriga.buckshot.engine.events

import cats.syntax.all.*
import com.zkerriga.buckshot.engine.BeliefState
import com.zkerriga.buckshot.engine.DealerBeliefChecks.missOnShellOut
import com.zkerriga.buckshot.engine.EngineError.*
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.game.events.Shot
import com.zkerriga.buckshot.game.events.outcome.Outcome.DealerWins
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.partitipant.Side
import com.zkerriga.buckshot.game.state.partitipant.Side.Player
import com.zkerriga.buckshot.game.state.shotgun.Shell
import com.zkerriga.types.Chance
import com.zkerriga.types.boundaries.optional
import com.zkerriga.types.boundaries.optional.*

case class PlayerShot(target: Side, shell: Shell)

object PlayerShot:
  def execute(state: GameState, shot: PlayerShot): V[DealerWins | ContinuableOutcome | GameState] =
    exec(state, shot): table =>
      optional:
        updateBelief(conditionBelief(old = state.public, table = table, shot, state.hidden.dealer.belief).?)

  def executeSimple(state: GameState, shot: PlayerShot): V[DealerWins | ContinuableOutcome | GameState] =
    exec(state, shot): _ =>
      updateBelief(state.hidden.dealer.belief).some

  private def exec(state: GameState, shot: PlayerShot) =
    val event = Shot(actor = Player, target = shot.target, shell = shot.shell)
    Execute.shot(state, event)(
      player = state.hidden.player.afterShellOut,
      notes = state.hidden.dealer.notes,
    )

  private def conditionBelief(
    old: TableState,
    table: TableState,
    shot: PlayerShot,
    belief: BeliefState[Revealed],
  ): Option[BeliefState[Revealed]] =
    belief.conditioning: revealed =>
      Chance.certainUnless(missOnShellOut(revealed, old = old.shotgun, updated = table.shotgun, out = shot.shell))

  private def updateBelief(belief: BeliefState[Revealed]): BeliefState[Revealed] =
    belief.update(_.afterShellOut)
