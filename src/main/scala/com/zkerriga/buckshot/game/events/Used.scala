package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.events.Used.ItemUse
import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.*
import com.zkerriga.buckshot.game.events.outcome.Outcome.*
import com.zkerriga.buckshot.game.state.GameState
import com.zkerriga.buckshot.game.state.items.*
import com.zkerriga.buckshot.game.state.partitipant.Side.*
import com.zkerriga.buckshot.game.state.partitipant.{Participant, Side}
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

  private case class WithoutItems(user: Participant, opponent: Participant)

  import ItemUse.*

  def execute(state: GameState, used: Used): V[GameOver | Reset | GameState] =
    for
      _ <- (state.turnOf == used.actor) trueOr WrongTurn
      withoutItems <- removeItems(state, used).toRight(MissingItems)
      outcome <- processEffects(state, withoutItems, used)
    yield outcome

  private def removeItems(state: GameState, used: Used): Option[WithoutItems] =
    val item = itemOf(used.item)
    def remove(user: Participant, opponent: Participant): Option[WithoutItems] =
      if used.stolen then
        for
          opponent <- opponent.without(item)
          user <- user.without(Adrenaline)
        yield WithoutItems(user = user, opponent = opponent)
      else
        for user <- user.without(item)
        yield WithoutItems(user = user, opponent = opponent)

    used.actor match
      case Player => remove(user = state.player, opponent = state.dealer)
      case Dealer => remove(user = state.dealer, opponent = state.player)

  private def itemOf(use: ItemUse): RegularItem = use match
    case Handcuffs => RegularItem.Handcuffs
    case _: MagnifyingGlass => RegularItem.MagnifyingGlass
    case _: Beer => RegularItem.Beer
    case Cigarettes => RegularItem.Cigarettes
    case Saw => RegularItem.Saw
    case Inverter => RegularItem.Inverter
    case _: BurnerPhone => RegularItem.BurnerPhone
    case _: Meds => RegularItem.Meds

  private def processEffects(state: GameState, itemless: WithoutItems, used: Used): V[GameOver | Reset | GameState] =
    used.item match
      case Handcuffs =>
        buildNextState(state, user = itemless.user, opponent = itemless.opponent.cuffed).ok

      case MagnifyingGlass(revealed) =>
        buildNextState(state, user = itemless.user.knowing(revealed, Shell1), opponent = itemless.opponent).ok

      case Beer(out) =>
        state.shotgun
          .shellOut(out)
          .map:
            case Some(shotgun) => buildNextState(state, itemless, shotgun)
            case None =>
              Reset.of(
                maxHealth = state.maxHealth,
                player = ???,
                dealer = ???,
              )

      case Cigarettes => ???
      case Saw => ???
      case Inverter => ???
      case BurnerPhone(revealed, at) => ???
      case Meds(good) => ???

  private def buildNextState(state: GameState, user: Participant, opponent: Participant): GameState =
    state.turnOf match
      case Player => state.copy(player = user, dealer = opponent)
      case Dealer => state.copy(dealer = user, player = opponent)

  private def buildNextState(state: GameState, itemless: WithoutItems, shotgun: Shotgun): GameState =
    buildNextState(state, user = itemless.user, opponent = itemless.opponent).copy(shotgun = shotgun)
