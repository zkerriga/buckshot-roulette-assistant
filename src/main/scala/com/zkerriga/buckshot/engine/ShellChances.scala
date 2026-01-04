package com.zkerriga.buckshot.engine

import com.zkerriga.buckshot.engine.state.{GameState, Revealed}
import com.zkerriga.buckshot.game.all.*

object ShellChances {
  def consideringEverything(state: GameState, at: SeqNr): Distribution[Shell] =
    state.hidden.dealer.belief.getDistribution.flatMap { dealer =>
      shellAt(state.public.shotgun, state.hidden.player.revealed, dealer, at)
    }.deduplicate

  def shellAt(shotgun: Shotgun, player: Revealed, dealer: Revealed, at: SeqNr): Distribution[Shell] =
    val common: Revealed = player.combineWith(dealer)
    common.get(at) match
      case Some(alreadyKnown) => Distribution.deterministic(alreadyKnown)
      case None =>
        shotgun.live
          .minus(common.count(Live))
          .fold(Distribution.deterministic(Blank)): live =>
            shotgun.blank
              .minus(common.count(Blank))
              .fold(Distribution.deterministic(Live)): blank =>
                Distribution.weighted(
                  live -> Live,
                  blank -> Blank,
                )
}
