package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.Engine
import com.zkerriga.buckshot.engine.Engine.{DealerPrediction, EventReply}
import com.zkerriga.buckshot.engine.events.PlayerUsed
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.{Shot, Used}
import com.zkerriga.buckshot.journal.AppLog.Logging

object GameWindow extends Logging:
  def window(dialogs: Dialogs, engine: Engine): Window =
    val window = BasicWindow("Buckshot Roulette Assistant")
    val content = Panel(LinearLayout(Direction.VERTICAL)).withAll(
      composition(dialogs, engine),
      QuitButton.render(window).setLayoutData(LinearLayout.createLayoutData(LinearLayout.Alignment.End)),
    )
    window.setComponent(content)
    window

  private def composition(dialogs: Dialogs, engine: Engine): Panel =
    engine.getState match
      case Left(error) => Panel().withAll(Label(error))
      case Right(game) =>
        val gameStateDynamic = DynamicComponent.updatable(game)(GameStateComponent.render)
        val controlDynamic = DynamicComponent.selfUpdatable[(GameState, Option[DealerPrediction])]((game, None)) {
          case ((game, dealerPrediction), update) =>
            ControlComponent.render(
              game,
              dealerPrediction,
              new InputComponent.Submit {
                def event(event: Engine.Event): Unit = {
                  log.debug(s"submitted $event")
                  val result = engine.process(event)
                  result match {
                    case Left(errorText) => dialogs.showError(errorText)
                    case Right(reply) =>
                      reply match {
                        case EventReply.NewState(state, dealer) =>
                          log.debug(s"updating components to $state")
                          gameStateDynamic.update(state)
                          update.update((state, dealer))
                          log.debug("all components are updated with new state")
                        case EventReply.GameOver(winner) =>
                          dialogs.showGameOutcome(winner)
                        case EventReply.ShotgunReset(reset) =>
                          dialogs.showReset(reset)
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
