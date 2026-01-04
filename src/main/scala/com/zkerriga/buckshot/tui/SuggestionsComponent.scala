package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.game.all.*

object SuggestionsComponent:
  def render(state: GameState): Component =
    Panel(LinearLayout(Direction.VERTICAL))
      .withSeq(
        Seq(
          nextShellExpectation(state),
        ),
      )
      .withBorder(Borders.singleLine("Suggestions"))

  def nextShellExpectation(state: GameState): Panel =
    Panel(GridLayout(2)).withAll(
      Label("Next shell chances:"),
      Panel(LinearLayout(Direction.VERTICAL)).withAll(
        Label("Live").setBackgroundColor(TextColor.ANSI.RED),
        Label("Blank").setBackgroundColor(TextColor.ANSI.BLUE),
        // todo: calculate chances
      ),
    )
