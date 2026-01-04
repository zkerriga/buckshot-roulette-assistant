package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.accessors.{Opposition, Parties}
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.*
import com.zkerriga.buckshot.game.events.outcome.Outcome.*
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.items.*
import com.zkerriga.buckshot.game.state.partitipant.Items.ItemOn
import com.zkerriga.buckshot.game.state.partitipant.Side.*
import com.zkerriga.buckshot.game.state.partitipant.{Damage, Heal, Participant, Side}
import com.zkerriga.buckshot.game.state.shotgun.{Shell, Shotgun}

case class Used(actor: Side, item: ItemUse, on: Slot, viaAdrenalineOn: Option[Slot])

object Used:
  private case class Participants(user: Participant, opponent: Participant)
  private case class Updated(participants: Participants, shotgun: Shotgun)

  def execute(state: TableState, used: Used): V[GameOver | Reset | TableState] =
    for
      _ <- (state.turn == used.actor) trueOr WrongTurn
      (opposition, parties) = defineRelations(used.actor)
      withoutItems <- removeItems(state, used)(using opposition)
      result <- processEffects(state, withoutItems, used)(using parties)
    yield result match
      case outcome: (GameOver | Reset) => outcome
      case updated: Updated => buildNextState(state, updated)(using parties)

  private def defineRelations(actor: Side): (Opposition[TableState], Parties[Participants]) =
    actor match
      case Player =>
        (
          Opposition.of(actorIs = _.player, opponentIs = _.dealer),
          Parties.of(dealerIs = _.opponent, playerIs = _.user),
        )
      case Dealer =>
        (
          Opposition.of(actorIs = _.dealer, opponentIs = _.player),
          Parties.of(dealerIs = _.user, playerIs = _.opponent),
        )

  private def removeItems(state: TableState, used: Used)(using Opposition[TableState]): V[Participants] =
    val item = used.item.toItem
    used.viaAdrenalineOn match
      case Some(adrenalineSlot) =>
        for
          _ <- state.actor.items.on(adrenalineSlot).contains(Adrenaline) trueOr MissingItem
          user = state.actor.withoutAdrenaline(adrenalineSlot)
          _ <- state.opponent.items.on(used.on).contains(item) trueOr MissingItem
          opponent = state.opponent.without(ItemOn(item, used.on))
        yield Participants(user = user, opponent = opponent)
      case None =>
        for _ <- state.actor.items.on(used.on).contains(item) trueOr MissingItem
        yield Participants(user = state.actor.without(ItemOn(item, used.on)), opponent = state.opponent)

  private def processEffects(
    state: TableState,
    itemless: Participants,
    used: Used,
  )(using Parties[Participants]): V[GameOver | Reset | Updated] =
    used.item match
      case ItemUse.Handcuffs =>
        for opponent <- itemless.opponent.cuffed
        yield Updated(itemless.copy(opponent = opponent), state.shotgun)

      case ItemUse.MagnifyingGlass =>
        Updated(itemless, state.shotgun).ok

      case ItemUse.Beer(out) =>
        state.shotgun
          .shellOut(out)
          .map:
            case Some(shotgun) =>
              Updated(itemless, shotgun)
            case None =>
              Reset.of(
                maxHealth = state.maxHealth,
                dealer = itemless.dealer,
                player = itemless.player,
              )

      case ItemUse.Cigarettes =>
        Updated(
          itemless.copy(user = itemless.user.healed(Heal.Single, state.maxHealth)),
          state.shotgun,
        ).ok

      case ItemUse.Saw =>
        state.shotgun.sawApplied.map: shotgun =>
          Updated(itemless, shotgun)

      case ItemUse.Inverter =>
        Updated(itemless, state.shotgun.inverterApplied).ok

      case ItemUse.BurnerPhone =>
        Updated(itemless, state.shotgun).ok

      case ItemUse.Meds(good) =>
        (
          if good then
            Updated(
              itemless.copy(user = itemless.user.healed(Heal.Double, state.maxHealth)),
              state.shotgun,
            )
          else
            itemless.user.damaged(Damage.Single) match
              case Some(user) => Updated(itemless.copy(user = user), state.shotgun)
              case None =>
                used.actor match
                  case Player => DealerWins
                  case Dealer =>
                    PlayerWins(
                      player = itemless.player.items,
                      dealer = itemless.dealer.items,
                    )
        ).ok

  private def buildNextState(state: TableState, updated: Updated)(using Parties[Participants]): TableState =
    state.copy(
      dealer = updated.participants.dealer,
      shotgun = updated.shotgun,
      player = updated.participants.player,
    )
