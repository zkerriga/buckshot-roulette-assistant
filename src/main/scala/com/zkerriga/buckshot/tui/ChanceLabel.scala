package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.Label
import com.zkerriga.types.Chance

object ChanceLabel:
  def renderUncertain(chance: Chance): Label =
    Label(if chance == Chance.Certain then "" else chance.show)

  def render(chance: Chance): Label =
    Label(chance.show)
