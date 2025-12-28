package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.engine.BeliefState
import com.zkerriga.buckshot.engine.state.{GameState, Knowledge}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Shot
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.V
import com.zkerriga.buckshot.game.events.outcome.Outcome.{GameOver, Reset}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.partitipant.Side.Player
import com.zkerriga.types.Chance

object PlayerShot:
  def execute(state: GameState, shot: Shot[Player.type]): V[GameOver | Reset | GameState] =
    Shot
      .execute(state.public, shot)
      .map:
        case outcome: (GameOver | Reset) => outcome
        case table: TableState =>
          val dealerKnowledgeBelief = state.knowledge.dealer
            .conditioning: revealed =>
              Chance.binary(revealed.get(Shell1).forall(_ == shot.shell))
            .transform: revealed =>
              BeliefState.deterministic(revealed.afterShellOut)
            .conditioning: revealed =>
              Chance.binary(revealed.count(Live) <= table.shotgun.live && revealed.count(Blank) <= table.shotgun.blank)

          GameState(
            public = table,
            knowledge = Knowledge(
              dealer = dealerKnowledgeBelief,
              player = state.knowledge.player.afterShellOut,
            ),
          )
