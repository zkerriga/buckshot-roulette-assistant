package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.Engine

object GameWindow:
  def window(engine: Engine): Window =
    val window = BasicWindow("Buckshot Roulette Assistant")
    val content = Panel(LinearLayout(Direction.VERTICAL)).withAll(
      composition(engine),
      QuitButton.render(window).setLayoutData(LinearLayout.createLayoutData(LinearLayout.Alignment.End)),
    )
    window.setComponent(content)
    window

  def composition(engine: Engine): Panel =
    engine.getState match
      case Left(error) => Panel().withAll(Label(error))
      case Right(game) =>
        Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
          GameStateComponent.render(game),
          EmptySpace(),
          ControlComponent.render(),
        )
