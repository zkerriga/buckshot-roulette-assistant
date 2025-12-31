package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.Engine
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.journal.AppLog.Logging

object ControlComponent extends Logging:
  def render(engine: Engine, initial: GameState, stateComponent: DynamicComponent.Update[GameState]): Component =
    Panel(LinearLayout(Direction.VERTICAL)).withAll(
      DealerPredictionComponent.render(None), // todo: how to update?
      InputComponent.render(initial.public, event => log.trace(s"submitted $event")),
      suggestions(),
    )

  private def suggestions(): Component = Panel()
