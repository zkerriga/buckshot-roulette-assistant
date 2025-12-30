package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.engine.ai.DealerAi
import com.zkerriga.buckshot.engine.ai.DealerAi.Action
import com.zkerriga.buckshot.engine.state.{GameState, Knowledge, Revealed}
import com.zkerriga.buckshot.engine.{BeliefState, Distribution}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Used
import com.zkerriga.buckshot.game.events.Used.ItemUse
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.V
import com.zkerriga.buckshot.game.events.outcome.Outcome.{GameOver, Reset}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.types.Chance

object DealerUsed:
  def execute(state: GameState, used: Used[Dealer.type]): V[GameOver | Reset | GameState] =
    Used
      .execute(state.public, used)
      .map:
        case outcome: (GameOver | Reset) => outcome
        case table: TableState =>
          GameState(
            public = table,
            knowledge = Knowledge(
              dealer = updateDealerKnowledge(state, used),
              player = updatePlayerKnowledge(state.knowledge.player, used.item),
            ),
          )

  private def updateDealerKnowledge(state: GameState, used: Used[Dealer.type]): BeliefState[Revealed] =
    // todo: would they use this item?
    // if BurnerPhone or Glass, make new assumptions
    val adjusted = state.knowledge.dealer
      .conditioning: knowledge =>
        DealerAi.next(state.public, knowledge) match
          case Action.Use(item, steal) => Chance.certainWhen(item == used.item && steal == used.stolen)
          case Action.Shoot(_) => Chance.NoChance
          case Action.Guess(live, Action.Shoot(Dealer)) =>
            live match
              case Action.Use(Saw, false) => Chance.certainWhen(used.item == Saw && !used.stolen) and Chance.CoinFlip
              case Action.Shoot(Player) => Chance.NoChance
    used.item match
      case ItemUse.MagnifyingGlass => ???
      case ItemUse.BurnerPhone => ???
      case ItemUse.Beer(_) => adjusted.update(_.afterShellOut)
      case _ => adjusted

  private def updatePlayerKnowledge(knowledge: Revealed, item: ItemUse): Revealed =
    item match
      case _: ItemUse.Beer => knowledge.afterShellOut
      case _ => knowledge
