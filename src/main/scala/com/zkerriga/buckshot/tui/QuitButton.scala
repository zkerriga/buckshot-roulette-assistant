package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.{Button, Window}

object QuitButton:
  def render(window: Window): Button =
    Button(
      "Quit",
      () => window.close(),
    )
