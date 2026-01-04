package com.zkerriga.buckshot

import com.googlecode.lanterna.gui2.*
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import com.zkerriga.buckshot.engine.Engine
import com.zkerriga.buckshot.journal.AppLog.Logging
import com.zkerriga.buckshot.tui.{ColorScheme, Dialogs, GameWindow, NewGameWindow}
import com.zkerriga.types.Ref

object Main extends Logging:
  private inline val DebugGameView = false

  def main(args: Array[String]): Unit =
    val screen = DefaultTerminalFactory()
      .setPreferTerminalEmulator(false)
      .setForceTextTerminal(true)
      .createScreen()

    screen.startScreen()

    val textGUI = MultiWindowTextGUI(screen)
    textGUI.setTheme(ColorScheme.Default)

    try {
      val engine: Ref[Option[Engine]] = Ref.of(None)

      if DebugGameView then engine.set(Some(Engine.start(TestState.state)))
      else
        log.trace("starting new game window")
        val newGameWindow = NewGameWindow.window: state =>
          engine.set(Some(Engine.start(state)))
        textGUI.addWindowAndWait(newGameWindow)
        log.trace("setup window closed")

      engine.get match
        case None => () // quit
        case Some(engine) =>
          log.trace("starting game window")
          val gameWindow = GameWindow.window(Dialogs(textGUI), engine)
          textGUI.addWindowAndWait(gameWindow)
          log.trace("game window closed")
    } catch {
      case exception: Throwable => log.error(s"flow failed with $exception")
    }

    screen.stopScreen()
