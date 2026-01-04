package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.Engine
import com.zkerriga.buckshot.engine.state.GameState

object ControlComponent:
  def render(state: GameState, engine: Engine, submit: InputComponent.Submit): Component =
    Panel(LinearLayout(Direction.VERTICAL)).withAll(
      DealerPredictionComponent.render(state, engine),
      InputComponent.render(state.public, submit),
      SuggestionsComponent.render(state, engine),
    )
