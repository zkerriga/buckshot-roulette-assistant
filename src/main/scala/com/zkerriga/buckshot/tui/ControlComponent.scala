package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*

object ControlComponent:
  def render(): Component =
    Panel(LinearLayout(Direction.VERTICAL)).withAll(
      Label("Input event:"),
      TextBox(),
    )
