package com.zkerriga.buckshot.game.state.partitipant

import com.zkerriga.buckshot.game.events.outcome.StateError.*
import com.zkerriga.types.steps.ResultExtension.*
import steps.result.Result

enum Hands:
  case Free, CuffedForTwoShots, CuffedForOneShot

object Hands:
  extension (hands: Hands)
    def free: Boolean = hands == Free

    def cuffed(using raise: Raise[HandsAlreadyCuffed.type]): Hands =
      if free then CuffedForTwoShots
      else raise.raise(HandsAlreadyCuffed)

    def afterTurn: Hands = hands match
      case Free | CuffedForOneShot => Free
      case CuffedForTwoShots => CuffedForOneShot
