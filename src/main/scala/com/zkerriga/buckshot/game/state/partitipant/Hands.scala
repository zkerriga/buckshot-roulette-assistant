package com.zkerriga.buckshot.game.state.partitipant

import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.*

enum Hands:
  case Free, CuffedForTwoShots, CuffedForOneShot

object Hands:
  extension (hands: Hands)
    def free: Boolean = hands == Free

    def cuffed: V[Hands] =
      if free then CuffedForTwoShots.ok
      else HandsAlreadyCuffed.lift

    def afterShot: Hands = hands match
      case Free | CuffedForOneShot => Free
      case CuffedForTwoShots => CuffedForOneShot
