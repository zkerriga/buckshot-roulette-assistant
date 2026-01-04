package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.game.events.outcome.Outcome.{PlayerWins, Reset}
import com.zkerriga.buckshot.game.state.items.Slot

enum ContinuableOutcome:
  case WinDetails(win: PlayerWins, dealerItemGroups: List[Set[Slot]])
  case ResetDetails(reset: Reset, dealerItemGroups: List[Set[Slot]])
