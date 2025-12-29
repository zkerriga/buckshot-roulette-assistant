package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.engine.BeliefState
import com.zkerriga.buckshot.engine.DealerBeliefChecks.{missOnGlassReveal, missOnPhoneReveal, missOnShellOut}
import com.zkerriga.buckshot.engine.events.PlayerUsed.ItemUse
import com.zkerriga.buckshot.engine.state.{GameState, Knowledge}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Used
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.V
import com.zkerriga.buckshot.game.events.outcome.Outcome.{GameOver, Reset}
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.partitipant.Side.Player
import com.zkerriga.types.Chance

case class PlayerUsed(item: ItemUse, stolen: Boolean)

object PlayerUsed:
  enum ItemUse:
    case Handcuffs
    case MagnifyingGlass(revealed: Shell)
    case Beer(out: Shell)
    case Cigarettes
    case Saw
    case Inverter
    case BurnerPhone(revealed: Shell, at: SeqNr)
    case Meds(good: Boolean)

  def execute(state: GameState, used: PlayerUsed): V[GameOver | Reset | GameState] =
    Used
      .execute(state.public, Used(Player, ItemUse.asPublic(used.item), used.stolen))
      .map:
        case outcome: (GameOver | Reset) => outcome
        case table: TableState =>
          import state.knowledge.*
          val updatedKnowledge =
            used.item match
              case ItemUse.MagnifyingGlass(revealed) =>
                Knowledge(
                  dealer = dealer.conditioning: knowledge =>
                    Chance.certainUnless:
                      missOnGlassReveal(knowledge, table.shotgun, revealed)
                  ,
                  player = player.revealed(revealed, Shell1),
                )

              case ItemUse.BurnerPhone(revealed, at) =>
                Knowledge(
                  dealer = dealer.conditioning: knowledge =>
                    Chance.certainUnless:
                      missOnPhoneReveal(knowledge, table.shotgun, revealed, at)
                  ,
                  player = player.revealed(revealed, at),
                )

              case ItemUse.Beer(out) =>
                Knowledge(
                  dealer = dealer
                    .conditioning: knowledge =>
                      Chance.certainUnless:
                        missOnShellOut(knowledge, old = state.shotgun, updated = table.shotgun, out = out)
                    .transform: knowledge =>
                      BeliefState.deterministic(knowledge.afterShellOut),
                  player = player.afterShellOut,
                )

              case ItemUse.Handcuffs | ItemUse.Cigarettes | ItemUse.Saw | ItemUse.Inverter | _: ItemUse.Meds =>
                state.knowledge

          GameState(
            public = table,
            knowledge = updatedKnowledge,
          )

  private object ItemUse:
    val asPublic: ItemUse => Used.ItemUse =
      case ItemUse.Handcuffs => Used.ItemUse.Handcuffs
      case ItemUse.MagnifyingGlass(revealed) => Used.ItemUse.MagnifyingGlass
      case ItemUse.Beer(out) => Used.ItemUse.Beer(out)
      case ItemUse.Cigarettes => Used.ItemUse.Cigarettes
      case ItemUse.Saw => Used.ItemUse.Saw
      case ItemUse.Inverter => Used.ItemUse.Inverter
      case ItemUse.BurnerPhone(revealed, at) => Used.ItemUse.BurnerPhone
      case ItemUse.Meds(good) => Used.ItemUse.Meds(good)
