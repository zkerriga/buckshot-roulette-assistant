package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.accessors.{Opposition, Parties}
import com.zkerriga.buckshot.game.events.Used.ItemUse
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.*
import com.zkerriga.buckshot.game.events.outcome.Outcome.*
import com.zkerriga.buckshot.game.state.GameState
import com.zkerriga.buckshot.game.state.items.*
import com.zkerriga.buckshot.game.state.partitipant.Side.*
import com.zkerriga.buckshot.game.state.partitipant.{Damage, Heal, Participant, Side}
import com.zkerriga.buckshot.game.state.shotgun.SeqNr.Shell1
import com.zkerriga.buckshot.game.state.shotgun.{SeqNr, Shell, Shotgun}

case class Used(actor: Side, item: ItemUse, stolen: Boolean)

object Used:
  enum ItemUse:
    case Handcuffs
    case MagnifyingGlass(revealed: Shell)
    case Beer(out: Shell)
    case Cigarettes
    case Saw
    case Inverter
    case BurnerPhone(revealed: Shell, at: SeqNr)
    case Meds(good: Boolean)

  private case class Participants(user: Participant, opponent: Participant)
  private case class Updated(participants: Participants, shotgun: Shotgun)

  import ItemUse.*

  def execute(state: GameState, used: Used): V[GameOver | Reset | GameState] =
    for
      _ <- (state.turnOf == used.actor) trueOr WrongTurn
      (opposition, parties) = defineRelations(used)
      withoutItems <- removeItems(state, used)(using opposition)
      result <- processEffects(state, withoutItems, used)(using parties)
    yield result match
      case outcome: (GameOver | Reset) => outcome
      case updated: Updated => buildNextState(state, updated)(using parties)

  private def defineRelations(used: Used): (Opposition[GameState], Parties[Participants]) =
    used.actor match
      case Player =>
        (
          Opposition.of(actorIs = _.player, opponentIs = _.dealer),
          Parties.of(playerIs = _.user, dealerIs = _.opponent),
        )
      case Dealer =>
        (
          Opposition.of(actorIs = _.dealer, opponentIs = _.player),
          Parties.of(playerIs = _.opponent, dealerIs = _.user),
        )

  private def removeItems(state: GameState, used: Used)(using Opposition[GameState]): V[Participants] =
    val item = itemOf(used.item)
    if used.stolen then
      for
        user <- state.actor.without(Adrenaline)
        opponent <- state.opponent.without(item)
      yield Participants(user = user, opponent = opponent)
    else
      for user <- state.actor.without(item)
      yield Participants(user = user, opponent = state.opponent)

  private def itemOf(use: ItemUse): RegularItem = use match
    case Handcuffs => RegularItem.Handcuffs
    case _: MagnifyingGlass => RegularItem.MagnifyingGlass
    case _: Beer => RegularItem.Beer
    case Cigarettes => RegularItem.Cigarettes
    case Saw => RegularItem.Saw
    case Inverter => RegularItem.Inverter
    case _: BurnerPhone => RegularItem.BurnerPhone
    case _: Meds => RegularItem.Meds

  private def processEffects(
    state: GameState,
    itemless: Participants,
    used: Used,
  )(using Parties[Participants]): V[GameOver | Reset | Updated] =
    used.item match
      case Handcuffs =>
        for opponent <- itemless.opponent.cuffed
        yield Updated(itemless.copy(opponent = opponent), state.shotgun)

      case MagnifyingGlass(revealed) =>
        Updated(itemless.copy(user = itemless.user.knowing(revealed, Shell1)), state.shotgun).ok

      case Beer(out) =>
        state.shotgun
          .shellOut(out)
          .map:
            case Some(shotgun) =>
              Updated(
                Participants(user = itemless.user.afterShellOut, opponent = itemless.opponent.afterShellOut),
                shotgun,
              )
            case None =>
              Reset.of(
                maxHealth = state.maxHealth,
                player = itemless.player,
                dealer = itemless.dealer,
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

      case BurnerPhone(revealed, at) =>
        Updated(
          itemless.copy(user = itemless.user.knowing(revealed, at)),
          state.shotgun,
        ).ok

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

  private def buildNextState(state: GameState, updated: Updated)(using Parties[Participants]): GameState =
    state.copy(
      player = updated.participants.player,
      dealer = updated.participants.dealer,
      shotgun = updated.shotgun,
    )
