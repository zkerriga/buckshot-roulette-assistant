package com.zkerriga.buckshot

import com.googlecode.lanterna.gui2.*
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import com.zkerriga.buckshot.game.state.GameState
import com.zkerriga.buckshot.tui.{ColorScheme, GameWindow, SetupWindow}

import java.util.concurrent.atomic.AtomicReference

object Main:
  def main(args: Array[String]): Unit =
    val screen = DefaultTerminalFactory()
      .setPreferTerminalEmulator(false)
      .setForceTextTerminal(true)
      .createScreen()

    screen.startScreen()

    val textGUI = MultiWindowTextGUI(screen)
    textGUI.setTheme(ColorScheme.Default)

    val stateRef: AtomicReference[Option[GameState]] = AtomicReference(None)
    val setupWindow = SetupWindow.window(state => stateRef.set(Some(state)))
    textGUI.addWindowAndWait(setupWindow)

    stateRef.get() match
      case None => () // quit
      case Some(state) =>
        val gameWindow = GameWindow.window(state)
        textGUI.addWindowAndWait(gameWindow)

    screen.stopScreen()
