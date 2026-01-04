package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.Engine
import com.zkerriga.buckshot.engine.Engine.EventReply
import com.zkerriga.buckshot.engine.events.ContinuableOutcome
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.game.events.outcome.Outcome.{PlayerWins, Reset}
import com.zkerriga.buckshot.game.state.partitipant.Side.{Dealer, Player}
import com.zkerriga.buckshot.journal.AppLog.Logging

object GameWindow extends Logging:
  def window(dialogs: Dialogs, engine: Engine, setContinuation: ContinuableOutcome => Unit): Window =
    val window = BasicWindow("Buckshot Roulette Assistant")
    val content = Panel(LinearLayout(Direction.VERTICAL)).withAll(
      composition(
        dialogs,
        engine,
        outcome => {
          setContinuation(outcome)
          window.close()
        },
      ),
      QuitButton.render(window).setLayoutData(LinearLayout.createLayoutData(LinearLayout.Alignment.End)),
    )
    window.setComponent(content)
    window

  private def composition(dialogs: Dialogs, engine: Engine, setContinuation: ContinuableOutcome => Unit): Panel =
    engine.getState match
      case Left(error) => Panel().withAll(Label(error))
      case Right(game) =>
        val gameStateDynamic = DynamicComponent.updatable(game)(GameStateComponent.render)
        val controlDynamic = DynamicComponent.selfUpdatable[GameState](game) { (game, update) =>
          ControlComponent.render(
            game,
            engine,
            new InputComponent.Submit {
              def event(event: Engine.Event): Unit = {
                log.debug(s"submitted $event")
                val result = engine.process(event)
                result match {
                  case Left(errorText) => dialogs.showError(errorText)
                  case Right(reply) =>
                    reply match {
                      case EventReply.NewState(state) =>
                        log.debug(s"updating components to $state")
                        gameStateDynamic.update(state)
                        update.update(state)
                        log.trace("all components are updated with new state")
                      case EventReply.GameOver(outcome) =>
                        outcome match {
                          case Some(playerWins) =>
                            dialogs.showGameOutcome(Player)
                            log.trace(s"setting continuation with $playerWins")
                            setContinuation(playerWins)
                          case None =>
                            dialogs.showGameOutcome(Dealer)
                        }
                      case EventReply.ShotgunReset(reset) =>
                        dialogs.showReset(reset)
                        log.trace(s"setting continuation with $reset")
                        setContinuation(reset)
                    }
                }
              }
            },
          )
        }
        Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
          gameStateDynamic.component,
          EmptySpace(),
          controlDynamic.component,
        )
