package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.Engine.DealerPrediction
import com.zkerriga.buckshot.engine.state.GameState

object ControlComponent:
  def render(state: GameState, dealer: Option[DealerPrediction], submit: InputComponent.Submit): Component =
    Panel(LinearLayout(Direction.VERTICAL)).withAll(
      DealerPredictionComponent.render(state.public, dealer),
      InputComponent.render(state.public, submit),
      SuggestionsComponent.render(state),
    )
