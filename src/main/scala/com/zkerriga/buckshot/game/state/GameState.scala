package com.zkerriga.buckshot.game.state

import com.zkerriga.buckshot.game.state.partitipant.Participant
import com.zkerriga.buckshot.game.state.shotgun.Shotgun

case class GameState(
  maxHealth: HealthLimit,
  shotgun: Shotgun,
  player: Participant,
  dealer: Participant,
  turn: CurrentTurn,
)
