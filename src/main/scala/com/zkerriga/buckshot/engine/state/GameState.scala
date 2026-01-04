package com.zkerriga.buckshot.engine.state

import com.zkerriga.buckshot.engine.Printer
import com.zkerriga.buckshot.game.state.TableState

case class GameState(
  public: TableState,
  hidden: PrivateStates,
):
  export public.*

  override def toString: String =
    Printer.print(public) + " | " + hidden.toString
