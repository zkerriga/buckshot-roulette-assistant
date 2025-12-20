package com.zkerriga.buckshot.game.state.shotgun

enum Shell:
  case Live, Blank

object Shell:
  extension (shell: Shell)
    def inverted: Shell = shell match
      case Shell.Live => Shell.Blank
      case Shell.Blank => Shell.Live
