package com.zkerriga.buckshot.engine

import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.engine.state.Revealed

object DealerBeliefChecks:
  def missOnShellOut(knowledge: Revealed, old: Shotgun, updated: Shotgun, out: Shell): Boolean =
    valueMiss(knowledge, out.considering(old.effects), Shell1) ||
      distributionMiss(knowledge.except(Shell1), updated)

  def missOnGlassReveal(knowledge: Revealed, shotgun: Shotgun, revealed: Shell): Boolean =
    valueMiss(knowledge, revealed.considering(shotgun.effects), Shell1) ||
      distributionMiss(knowledge, shotgun)

  def missOnPhoneReveal(knowledge: Revealed, shotgun: Shotgun, revealed: Shell, at: SeqNr): Boolean =
    valueMiss(knowledge, revealed, at) ||
      distributionMiss(knowledge, shotgun)

  private def distributionMiss(revealed: Revealed, shotgun: Shotgun): Boolean =
    revealed.count(Live) > shotgun.live || revealed.count(Blank) > shotgun.blank

  private def valueMiss(revealed: Revealed, actual: Shell, at: SeqNr): Boolean =
    revealed.get(at).exists(_ != actual)
