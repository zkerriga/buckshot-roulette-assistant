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

  private def composition(engine: Engine): Panel =
    engine.getState match
      case Left(error) => Panel().withAll(Label(error))
      case Right(game) =>
        val gameStateDynamic = DynamicComponent.of(game, GameStateComponent.render)
        Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
          gameStateDynamic.component,
          EmptySpace(),
          ControlComponent.render(engine, game, gameStateDynamic),
        )
