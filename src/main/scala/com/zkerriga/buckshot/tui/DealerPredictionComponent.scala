package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.Engine.DealerPrediction

object DealerPredictionComponent:
  def render(actions: Option[DealerPrediction]): Component =
    actions match
      case Some(actions) => prediction(actions).withBorder(Borders.singleLine("Dealer Prediction"))
      case None => Panel()

  private def prediction(actions: DealerPrediction): Panel =
    Panel(LinearLayout(Direction.VERTICAL)).withSeq(
      // todo: stupid for now, implement later
      actions.actions.toString.sliding(40, 40).toSeq.map(line => Label(line)),
    )
