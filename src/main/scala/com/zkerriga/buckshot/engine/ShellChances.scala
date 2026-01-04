package com.zkerriga.buckshot.engine

import com.zkerriga.buckshot.engine.state.{GameState, Revealed}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state.shotgun
import com.zkerriga.types.Nat

object ShellChances {
  def consideringEverything(state: GameState, at: SeqNr): Distribution[Shell] =
    val distribution = state.hidden.dealer.belief.getDistribution
      .flatMap(dealer => shellAt(state.public.shotgun, state.hidden.player.revealed, dealer, at))
      .deduplicate
    at match {
      case Shell1 => distribution.map(_.considering(state.shotgun.effects))
      case _ => distribution
    }

  def shellAt(shotgun: Shotgun, player: Revealed, dealer: Revealed, at: SeqNr): Distribution[Shell] =
    val common: Revealed = player.combineWith(dealer)
    common.get(at) match
      case Some(alreadyKnown) => Distribution.deterministic(alreadyKnown)
      case None =>
        shotgun.live
          .minus(common.count(Live))
          .filterNot(_ == Nat[0])
          .fold(Distribution.deterministic(Blank)): live =>
            shotgun.blank
              .minus(common.count(Blank))
              .filterNot(_ == Nat[0])
              .fold(Distribution.deterministic(Live)): blank =>
                Distribution.weighted(
                  live -> Live,
                  blank -> Blank,
                )
}
