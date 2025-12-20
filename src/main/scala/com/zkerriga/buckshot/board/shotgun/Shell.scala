package com.zkerriga.buckshot.board.shotgun

enum Shell:
  case Live, Blank

object Shell:
  extension (shell: Shell)
    def invert: Shell = shell match
      case Shell.Live => Shell.Blank
      case Shell.Blank => Shell.Live
