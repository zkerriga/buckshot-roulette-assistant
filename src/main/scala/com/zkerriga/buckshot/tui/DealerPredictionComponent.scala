package com.zkerriga.buckshot.tui

import cats.Eq
import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.Distribution
import com.zkerriga.buckshot.engine.Engine.DealerPrediction
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.ai.DealerAi.Action
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state.items.Slot.Slot1
import com.zkerriga.buckshot.tui.ItemGridComponent.labelName
import com.zkerriga.types.Chance

object DealerPredictionComponent:
  def render(table: TableState, actions: Option[DealerPrediction]): Component =
    actions match
      case Some(actions) =>
        Panel(LinearLayout(Direction.VERTICAL))
          .withSeq(
            Seq(
              dealerMayUse(table.dealer.items, actions.possible),
              dealerMaySteal(table.player.items, actions.possible),
              dealerMayShoot(actions.possible),
            ).flatten,
          )
          .withBorder(Borders.singleLine("Dealer Prediction"))
      case None => Panel()

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
                    possible.chanceOf(DealerAi.Action.Use(ItemOn(item, slot)))(using Eq.fromUniversalEquals)
                  if chanceOfUsed == Chance.NoChance then
                    Label(item.labelName).setForegroundColor(TextColor.ANSI.BLACK_BRIGHT)
                  else
                    Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
                      Label(item.labelName),
                      ChanceLabel.render(chanceOfUsed),
                    )
              }
            case None => Label(" ")
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
                    possible.chanceOf(DealerAi.Action.Steal(ItemOn(item, slot)))(using Eq.fromUniversalEquals)
                  if chanceOfStolen == Chance.NoChance then
                    Label(item.labelName).setForegroundColor(TextColor.ANSI.BLACK_BRIGHT)
                  else
                    Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
                      Label(item.labelName),
                      ChanceLabel.render(chanceOfStolen),
                    )
              }
            case None => Label(" ")
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
        val chance = possible.chanceOf(DealerAi.Action.Shoot(side))(using Eq.fromUniversalEquals)
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
