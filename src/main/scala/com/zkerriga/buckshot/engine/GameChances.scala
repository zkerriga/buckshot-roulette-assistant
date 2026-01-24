package com.zkerriga.buckshot.engine

import cats.data.NonEmptySeq
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.state.GameState
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.types.Nat
import com.zkerriga.types.boundaries.optional
import com.zkerriga.types.boundaries.optional.*

object GameChances:
  val MedsGood: Distribution[Boolean] =
    Distribution.weighted(
      Nat[1] -> true,
      Nat[2] -> false,
    )

  def shellAt(state: GameState, at: SeqNr): Distribution[Shell] =
    (for
      dealerRevealed <- state.hidden.dealer.belief.getDistribution
      shell <- ShellChances.shellAt(
        shotgun = state.shotgun,
        player = state.hidden.player.revealed,
        dealer = dealerRevealed,
        at = at,
      )
    yield shell).deduplicate

  def nextShell(state: GameState): Distribution[Shell] =
    shellAt(state, Shell1).map(_.considering(state.shotgun.effects))

  def burnerPhonePosition(table: TableState): Option[Distribution[SeqNr]] =
    optional:
      val options = (table.shotgun.total minus Nat[1]).?
      val positions = NonEmptySeq.fromSeq(Seq(Shell2, Shell3, Shell4, Shell5, Shell6, Shell7, Shell8).take(options)).?
      Distribution.weighted(positions.map(Nat[1] -> _))

  def nextDealerAction(state: GameState): Distribution[DealerAi.Action] =
    (for
      revealed <- state.hidden.dealer.belief.getDistribution
      action <- DealerAi.next(state.public, state.hidden.dealer.notes, revealed)
    yield action).deduplicate
