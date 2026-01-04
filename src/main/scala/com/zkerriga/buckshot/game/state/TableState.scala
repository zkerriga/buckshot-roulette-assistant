package com.zkerriga.buckshot.game.state

import com.zkerriga.buckshot.game.events.outcome.Outcome.Reset
import com.zkerriga.buckshot.game.state.partitipant.Side.Player
import com.zkerriga.buckshot.game.state.partitipant.{Hands, Items, Participant, Side}
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

object TableState:
  def from(reset: Reset, shells: Shotgun.ShellDistribution, dealer: Items, player: Items): TableState =
    TableState(
      maxHealth = reset.maxHealth,
      turn = Player,
      dealer = Participant(
        health = reset.dealer.health,
        items = dealer,
        hands = Hands.Free,
      ),
      shotgun = Shotgun.fresh(live = shells.live, blank = shells.blank),
      player = Participant(
        health = reset.player.health,
        items = player,
        hands = Hands.Free,
      ),
    )
