package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.graphics.{SimpleTheme, Theme}

object ColorScheme:
  val Default: Theme = SimpleTheme.makeTheme(
    true, // activeIsBold
    TextColor.RGB(220, 220, 220), // base foreground
    TextColor.RGB(40, 40, 40), // base background
    TextColor.RGB(255, 200, 120), // editable foreground
    TextColor.RGB(60, 60, 60), // editable background
    TextColor.RGB(0, 0, 0), // selected foreground
    TextColor.RGB(180, 180, 180), // selected background
    TextColor.RGB(30, 30, 30), // GUI background
  )
