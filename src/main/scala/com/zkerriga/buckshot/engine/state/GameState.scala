package com.zkerriga.buckshot.engine.state

import com.zkerriga.buckshot.game.state.TableState

case class GameState(
  public: TableState,
  hidden: PrivateStates,
):
  export public.*
