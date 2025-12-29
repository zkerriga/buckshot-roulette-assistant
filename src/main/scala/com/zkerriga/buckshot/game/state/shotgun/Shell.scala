package com.zkerriga.buckshot.game.state.shotgun

enum Shell:
  case Live, Blank

object Shell:
  extension (shell: Shell)
    def inverted: Shell = shell match
      case Live => Blank
      case Blank => Live

    def considering(effects: Shotgun.Effects): Shell =
      if effects.inverted then shell.inverted else shell
