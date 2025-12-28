package com.zkerriga.buckshot.game.state

import com.zkerriga.buckshot.game.state.partitipant.{Participant, Side}
import com.zkerriga.buckshot.game.state.shotgun.Shotgun

/** public information about the table state
  */
case class TableState(
  maxHealth: HealthLimit,
  turn: Side,
  dealer: Participant,
  shotgun: Shotgun,
  player: Participant,
)
