package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.Engine
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state.shotgun

object SuggestionsComponent:
  def render(state: GameState, engine: Engine): Component =
    Panel(LinearLayout(Direction.VERTICAL))
      .withSeq(
        Seq(
          EmptySpace(),
          shellExpectations(state, engine),
        ),
      )
      .withBorder(Borders.singleLine("Suggestions"))

  def shellExpectations(state: GameState, engine: Engine): Panel = {
    import GridLayout.*
    Panel(GridLayout(3)).withSeq(
      Seq(
        Seq(
          Label("Shells:"),
          Label("Live").setBackgroundColor(TextColor.ANSI.RED),
          Label("Blank").setBackgroundColor(TextColor.ANSI.BLUE),
        ),
        Seq.fill(3)(Separator(Direction.HORIZONTAL).setLayoutData(createHorizontallyFilledLayoutData())),
        engine.calculateShellsChances(state).flatMap { (seqNr, distribution) =>
          Seq(
            Label(seqNr.name),
            ChanceLabel.render(distribution.chanceOf(Live)),
            ChanceLabel.render(distribution.chanceOf(Blank)),
          )
        },
      ).flatten,
    )
  }

  extension (seqNr: SeqNr)
    def name: String = seqNr match
      case Shell1 => "1-st"
      case Shell2 => "2-d"
      case Shell3 => "3-d"
      case Shell4 => "4-th"
      case Shell5 => "5-th"
      case Shell6 => "6-th"
      case Shell7 => "7-th"
      case Shell8 => "8-th"
