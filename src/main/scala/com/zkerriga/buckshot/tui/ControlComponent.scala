package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.state.GameState

object ControlComponent:
  def render(state: GameState, submit: InputComponent.Submit): Component =
    Panel(LinearLayout(Direction.VERTICAL)).withAll(
      DealerPredictionComponent.render(None), // todo: how to update?
      InputComponent.render(state.public, submit),
      suggestions(),
    )

  private def suggestions(): Component = Panel()
