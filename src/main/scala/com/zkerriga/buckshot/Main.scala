package com.zkerriga.buckshot

import com.googlecode.lanterna.gui2.*
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import com.zkerriga.buckshot.engine.Engine
import com.zkerriga.buckshot.journal.AppLog.Logging
import com.zkerriga.buckshot.tui.{ColorScheme, GameWindow, SetupWindow}
import com.zkerriga.types.Ref

object Main:
  def main(args: Array[String]): Unit =
    val screen = DefaultTerminalFactory()
      .setPreferTerminalEmulator(false)
      .setForceTextTerminal(true)
      .createScreen()

    screen.startScreen()

    val textGUI = MultiWindowTextGUI(screen)
    textGUI.setTheme(ColorScheme.Default)

    val engine: Ref[Option[Engine]] = Ref.of(None)
    val setupWindow = SetupWindow.window: table =>
      engine.set(Some(Engine.start(table)))

    textGUI.addWindowAndWait(setupWindow)
    engine.get match
      case None => () // quit
      case Some(engine) =>
        val gameWindow = GameWindow.window(engine)
        textGUI.addWindowAndWait(gameWindow)

    screen.stopScreen()
