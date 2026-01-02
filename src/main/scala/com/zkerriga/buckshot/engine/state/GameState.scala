package com.zkerriga.buckshot.engine.state

import com.zkerriga.buckshot.game.state.TableState

case class GameState(public: TableState, knowledge: Knowledge):
  export public.*

object GameState:
  def initial(state: TableState): GameState =
    GameState(
      public = state,
      knowledge = Knowledge.Empty,
    )
