package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.game.all.*

object GameWindow:
  def window(game: GameState): Window =
    val window = BasicWindow("Buckshot Roulette Assistant")
    val content = Panel(LinearLayout(Direction.VERTICAL)).withAll(
      composition(game),
      QuitButton.render(window).setLayoutData(LinearLayout.createLayoutData(LinearLayout.Alignment.End)),
    )
    window.setComponent(content)
    window

  def composition(game: GameState): Panel =
    Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
      GameStateComponent.render(game),
      EmptySpace(),
      ControlComponent.render(),
    )
