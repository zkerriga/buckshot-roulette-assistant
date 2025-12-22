package com.zkerriga.buckshot

import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.input.KeyType
import com.googlecode.lanterna.screen.TerminalScreen
import com.googlecode.lanterna.terminal.DefaultTerminalFactory

object Main:
  def main(args: Array[String]): Unit =
    val terminal =
      new DefaultTerminalFactory()
        .setPreferTerminalEmulator(false)
        .setForceTextTerminal(true)
        .createTerminal()

    val screen = TerminalScreen(terminal)

    def render(message: String): Unit =
      screen.clear()
      val tg: TextGraphics = screen.newTextGraphics()

      tg.putString(2, 1, "Buckshot Roulette Assistant")
      tg.putString(2, 3, "[1] Say Hello")
      tg.putString(2, 4, "[2] Exit")
      tg.putString(2, 6, s"Message: $message")

    screen.startScreen()
    screen.setCursorPosition(null) // hide cursor

    var running = true
    var message = "Press 1 or 2"

    while running do
      render(message)
      screen.refresh()

      val key = screen.readInput()
      key.getKeyType match
        case KeyType.Character =>
          key.getCharacter match
            case '1' =>
              message = "Hello ⚪"
            case '2' | 'q' =>
              running = false
            case _ => ()

        case KeyType.Escape =>
          running = false

        case _ => ()

    screen.stopScreen()
