package com.zkerriga.buckshot.engine.state

import com.zkerriga.buckshot.engine.BeliefState

case class Knowledge(
  dealer: BeliefState[Revealed],
  player: Revealed,
)

object Knowledge:
  val Empty: Knowledge = Knowledge(
    dealer = BeliefState.deterministic(Revealed.Nothing),
    player = Revealed.Nothing,
  )
