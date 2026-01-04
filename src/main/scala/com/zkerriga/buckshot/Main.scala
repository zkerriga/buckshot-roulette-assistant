package com.zkerriga.buckshot

import com.googlecode.lanterna.gui2.*
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import com.zkerriga.buckshot.engine.Engine
import com.zkerriga.buckshot.engine.events.ContinuableOutcome
import com.zkerriga.buckshot.journal.AppLog.Logging
import com.zkerriga.buckshot.tui.*
import com.zkerriga.types.Ref

import scala.annotation.tailrec

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
      val engineRef: Ref[Option[Engine]] = Ref.of(None)

      if DebugGameView then engineRef.set(Some(Engine.start(TestState.state)))
      else
        log.trace("starting new game window")
        val newGameWindow = NewGameWindow.window: state =>
          engineRef.set(Some(Engine.start(state)))
        textGUI.addWindowAndWait(newGameWindow)
        log.trace("setup window closed")

      engineRef.getAndSet(None) match
        case None => () // quit
        case Some(engine) =>
          val continuation: Ref[Option[ContinuableOutcome]] = Ref.of(None)

          log.trace("starting game window")
          val gameWindow = GameWindow.window(Dialogs(textGUI), engine, outcome => continuation.set(Some(outcome)))
          textGUI.addWindowAndWait(gameWindow)
          log.trace("game window closed")

          @tailrec
          def loop(): Unit =
            continuation.getAndSet(None) match {
              case None => () // quit
              case Some(outcome) =>
                val continueWindow =
                  ContinueGameWindow.window(outcome, state => engineRef.set(Some(Engine.start(state))))
                textGUI.addWindowAndWait(continueWindow)
                engineRef.getAndSet(None) match {
                  case None => () // quit
                  case Some(engine) =>
                    val gameWindow =
                      GameWindow.window(Dialogs(textGUI), engine, outcome => continuation.set(Some(outcome)))
                    textGUI.addWindowAndWait(gameWindow)
                    loop()
                }
            }

          log.debug("starting game loop")
          loop()
          log.debug("game loop is over")
    } catch {
      case exception: Throwable => log.error(s"flow failed with $exception")
    }

    screen.stopScreen()
