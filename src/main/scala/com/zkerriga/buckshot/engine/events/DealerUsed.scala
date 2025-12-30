package com.zkerriga.buckshot.engine.events

import cats.data.NonEmptySeq
import com.zkerriga.buckshot.engine.DealerBeliefChecks.missOnShellOut
import com.zkerriga.buckshot.engine.EngineError.*
import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.ai.DealerAi.Action
import com.zkerriga.buckshot.engine.state.{GameState, Knowledge, Revealed}
import com.zkerriga.buckshot.engine.{BeliefState, Distribution}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Used
import com.zkerriga.buckshot.game.events.Used.ItemUse
import com.zkerriga.buckshot.game.events.outcome.Outcome.{GameOver, Reset}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.types.{Chance, Nat}

object DealerUsed:
  def execute(state: GameState, used: Used[Dealer.type]): V[GameOver | Reset | GameState] =
    Used
      .execute(state.public, used)
      .flatMap:
        case outcome: (GameOver | Reset) => outcome.ok
        case table: TableState =>
          for adjustedDealerKnowledge <- state.knowledge.dealer.conditioning(condition(state, table, used))
          yield GameState(
            public = table,
            knowledge = Knowledge(
              dealer = transformDealer(state.knowledge.player, table, used.item)(adjustedDealerKnowledge),
              player = updatePlayerKnowledge(state.knowledge.player, used.item),
            ),
          )

  private def condition(oldState: GameState, table: TableState, used: Used[Dealer.type])(knowledge: Revealed): Chance =
    val beerMiss = used.item match
      case ItemUse.Beer(out) => missOnShellOut(knowledge, old = oldState.shotgun, updated = table.shotgun, out = out)
      case _ => false

    if beerMiss then Chance.NoChance
    else
      DealerAi.next(oldState.public, knowledge) match
        case Action.Use(item, steal) => Chance.certainWhen(item == used.item && steal == used.stolen)
        case Action.Shoot(_) => Chance.NoChance
        case Action.Guess(live, Action.Shoot(Dealer)) =>
          live match
            case Action.Use(Saw, false) => Chance.certainWhen(used.item == Saw && !used.stolen) and Chance.CoinFlip
            case Action.Shoot(Player) => Chance.NoChance

  private def transformDealer(
    player: Revealed,
    table: TableState,
    item: ItemUse,
  )(adjusted: BeliefState[Revealed]): BeliefState[Revealed] =
    item match
      case ItemUse.MagnifyingGlass =>
        adjusted.transform: dealerKnowledge =>
          shellAt(table, player = player, dealer = dealerKnowledge, at = Shell1).map: shell =>
            dealerKnowledge.revealed(shell, Shell1)

      case ItemUse.BurnerPhone =>
        adjusted.transform: dealerKnowledge =>
          (for
            options <- table.shotgun.total minus Nat[1]
            positions <- NonEmptySeq.fromSeq(Seq(Shell2, Shell3, Shell4, Shell5, Shell6, Shell7, Shell8).take(options))
          yield positions).fold(Distribution.deterministic(dealerKnowledge)): positions =>
            for
              seqNr <- Distribution.weighted(positions.map(Nat[1] -> _))
              shell <- shellAt(table, player = player, dealer = dealerKnowledge, at = seqNr)
            yield dealerKnowledge.revealed(shell, seqNr)

      case ItemUse.Beer(out) => adjusted.update(_.afterShellOut)
      case _ => adjusted

  private def shellAt(table: TableState, player: Revealed, dealer: Revealed, at: SeqNr): Distribution[Shell] =
    val common: Revealed = player combine dealer
    common.get(at) match
      case Some(alreadyKnown) => Distribution.deterministic(alreadyKnown)
      case None =>
        table.shotgun.live
          .minus(common.count(Live))
          .fold(Distribution.deterministic(Blank)): live =>
            table.shotgun.blank
              .minus(common.count(Blank))
              .fold(Distribution.deterministic(Live)): blank =>
                Distribution.weighted(
                  live -> Live,
                  blank -> Blank,
                )

  private def updatePlayerKnowledge(knowledge: Revealed, item: ItemUse): Revealed =
    item match
      case _: ItemUse.Beer => knowledge.afterShellOut
      case _ => knowledge
