package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.ai.DealerAi.Action
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.engine.{Distribution, Engine}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.tui.ItemGridComponent.{NoItem, labelName}
import com.zkerriga.types.Chance

object DealerPredictionComponent:
  def render(state: GameState, engine: Engine): Component =
    state.turn match
      case Player => Panel()
      case Dealer =>
        val prediction = engine.calculateDealerPrediction(state)
        Panel(GridLayout(1))
          .withSeq(
            Seq(
              dealerMayUse(state.dealer.items, prediction),
              dealerMaySteal(state.player.items, prediction),
              dealerMayShoot(prediction),
            ).flatten,
          )
          .withBorder(Borders.singleLine("Prediction"))

  private def dealerMayUse(items: Items, possible: Distribution[DealerAi.Action]): Option[Component] =
    Option.when(possible.exists {
      case Action.Use(_) => true
      case _ => false
    }) {
      Panel(LinearLayout(Direction.VERTICAL)).withAll(
        Label("Dealer may use:"),
        ItemGridComponent.render(items) { (itemOpt, slot) =>
          itemOpt match {
            case Some(item) =>
              item match {
                case Adrenaline =>
                  val canBeUsed = possible.exists {
                    case DealerAi.Action.Steal(_) => true
                    case _ => false
                  }
                  if canBeUsed then Label(Adrenaline.labelName)
                  else Label(Adrenaline.labelName).setForegroundColor(TextColor.ANSI.BLACK_BRIGHT)

                case item: RegularItem =>
                  val chanceOfUsed =
                    possible.chanceOf(DealerAi.Action.Use(ItemOn(item, slot)))
                  if chanceOfUsed == Chance.NoChance then
                    Label(item.labelName).setForegroundColor(TextColor.ANSI.BLACK_BRIGHT)
                  else
                    Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
                      Label(item.labelName),
                      ChanceLabel.render(chanceOfUsed),
                    )
              }
            case None => Label(NoItem)
          }
        },
      )
    }

  private def dealerMaySteal(items: Items, possible: Distribution[DealerAi.Action]): Option[Component] =
    Option.when(possible.exists {
      case Action.Steal(_) => true
      case _ => false
    }) {
      Panel(LinearLayout(Direction.VERTICAL)).withAll(
        Label("Dealer may steal:"),
        ItemGridComponent.render(items) { (itemOpt, slot) =>
          itemOpt match {
            case Some(item) =>
              item match {
                case Adrenaline => Label(Adrenaline.labelName).setForegroundColor(TextColor.ANSI.BLACK_BRIGHT)
                case item: RegularItem =>
                  val chanceOfStolen =
                    possible.chanceOf(DealerAi.Action.Steal(ItemOn(item, slot)))
                  if chanceOfStolen == Chance.NoChance then
                    Label(item.labelName).setForegroundColor(TextColor.ANSI.BLACK_BRIGHT)
                  else
                    Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
                      Label(item.labelName),
                      ChanceLabel.render(chanceOfStolen),
                    )
              }
            case None => Label(NoItem)
          }

        },
      )
    }

  private def dealerMayShoot(possible: Distribution[DealerAi.Action]): Option[Component] =
    Option.when(possible.exists {
      case Action.Shoot(_) => true
      case _ => false
    }) {
      def target(side: Side): Option[Component] = {
        val chance = possible.chanceOf(DealerAi.Action.Shoot(side))
        Option.unless(chance == Chance.NoChance) {
          Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
            Label(side.toString),
            ChanceLabel.render(chance),
          )
        }
      }

      Panel(LinearLayout(Direction.VERTICAL)).withAll(
        Label("Dealer may shoot:"),
        Panel(LinearLayout(Direction.VERTICAL)).withSeq(
          Seq(
            target(Player),
            target(Dealer),
          ).flatten,
        ),
      )
    }
