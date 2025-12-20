package com.zkerriga.buckshot.game.state

import com.zkerriga.buckshot.game.state.partitipant.{Participant, Side}
import com.zkerriga.buckshot.game.state.shotgun.Shotgun

case class GameState(
  maxHealth: HealthLimit,
  shotgun: Shotgun,
  player: Participant,
  dealer: Participant,
  turnOf: Side,
)
