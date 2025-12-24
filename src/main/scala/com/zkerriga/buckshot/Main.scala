package com.zkerriga.buckshot

import com.googlecode.lanterna.gui2.*
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import com.zkerriga.buckshot.tui.{ColorScheme, Render}

object Main:
  def main(args: Array[String]): Unit =
    val screen = DefaultTerminalFactory()
      .setPreferTerminalEmulator(false)
      .setForceTextTerminal(true)
      .createScreen()

    screen.startScreen()

    val textGUI = MultiWindowTextGUI(screen)
    textGUI.setTheme(ColorScheme.Default)
    val window = Render.window(Data.TestState)

    textGUI.addWindowAndWait(window)
    screen.stopScreen()
