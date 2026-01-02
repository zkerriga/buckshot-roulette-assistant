package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.accessors.{Opposition, Parties}
import com.zkerriga.buckshot.game.events.Used.ItemUse
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.*
import com.zkerriga.buckshot.game.events.outcome.Outcome.*
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.items.*
import com.zkerriga.buckshot.game.state.partitipant.Side.*
import com.zkerriga.buckshot.game.state.partitipant.{Damage, Heal, Participant, Side}
import com.zkerriga.buckshot.game.state.shotgun.{Shell, Shotgun}

case class Used[+Actor <: Side](actor: Actor, item: ItemUse, stolen: Boolean)

object Used:
  enum ItemUse:
    case Handcuffs
    case MagnifyingGlass
    case Beer(out: Shell)
    case Cigarettes
    case Saw
    case Inverter
    case BurnerPhone
    case Meds(good: Boolean)

  private case class Participants(user: Participant, opponent: Participant)
  private case class Updated(participants: Participants, shotgun: Shotgun)

  import ItemUse.*

  def execute(state: TableState, used: Used[Side]): V[GameOver | Reset | TableState] =
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

  private def removeItems(state: TableState, used: Used[Side])(using Opposition[TableState]): V[Participants] =
    val item = itemOf(used.item)
    if used.stolen then
      for
        user <- state.actor.without(Adrenaline)
        opponent <- state.opponent.without(item)
      yield Participants(user = user, opponent = opponent)
    else
      for user <- state.actor.without(item)
      yield Participants(user = user, opponent = state.opponent)

  def itemOf(use: ItemUse): RegularItem = use match
    case Handcuffs => RegularItem.Handcuffs
    case MagnifyingGlass => RegularItem.MagnifyingGlass
    case _: Beer => RegularItem.Beer
    case Cigarettes => RegularItem.Cigarettes
    case Saw => RegularItem.Saw
    case Inverter => RegularItem.Inverter
    case BurnerPhone => RegularItem.BurnerPhone
    case _: Meds => RegularItem.Meds

  private def processEffects(
    state: TableState,
    itemless: Participants,
    used: Used[Side],
  )(using Parties[Participants]): V[GameOver | Reset | Updated] =
    used.item match
      case Handcuffs =>
        for opponent <- itemless.opponent.cuffed
        yield Updated(itemless.copy(opponent = opponent), state.shotgun)

      case MagnifyingGlass =>
        Updated(itemless, state.shotgun).ok

      case Beer(out) =>
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

      case Cigarettes =>
        Updated(
          itemless.copy(user = itemless.user.healed(Heal.Single, state.maxHealth)),
          state.shotgun,
        ).ok

      case Saw =>
        state.shotgun.sawApplied.map: shotgun =>
          Updated(itemless, shotgun)

      case Inverter =>
        Updated(itemless, state.shotgun.inverterApplied).ok

      case BurnerPhone =>
        Updated(itemless, state.shotgun).ok

      case Meds(good) =>
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
