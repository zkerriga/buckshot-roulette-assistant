package com.zkerriga.buckshot.tui

import cats.syntax.all.*
import com.googlecode.lanterna.gui2.{Component, Label}
import com.zkerriga.buckshot.engine.events.PlayerUsed.ItemUse as FullItemUse
import com.zkerriga.buckshot.game.accessors.Opposition
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Used.ItemUse
import com.zkerriga.buckshot.game.state.items.Item
import com.zkerriga.types.Nat

import scala.NamedTuple.{AnyNamedTuple, Concat}

object InputComponent:
  enum Event:
    case DealerShot(target: Side, shell: Shell)
    case PlayerShot(target: Side, shell: Shell)
    case DealerUsed(item: ItemUse, stolen: Boolean)
    case PlayerUsed(item: FullItemUse, stolen: Boolean)

  trait Submit:
    def event(event: Event): Unit

  import InputButtonsComponent.*

  def render(game: TableState, submit: Submit): Component = {
    given Opposition[TableState] = game.turn match
      case Player => Opposition.of(actorIs = _.player, opponentIs = _.dealer)
      case Dealer => Opposition.of(actorIs = _.dealer, opponentIs = _.player)

    val actorItems = game.actor.items
    val opponentItems = game.opponent.items

    def cannotUseAnyItem: Boolean =
      actorItems.empty ||
        actorItems.asSet == Set(Adrenaline) && (opponentItems.empty || opponentItems.asSet == Set(Adrenaline))

    val initial: InputState.Choose[?, ?, Event] =
      if cannotUseAnyItem then
        InputState.Choose(
          undo = None,
          acc = Accumulated((actor = game.turn), List(ShotChoice.label)),
          select = TargetSelect,
        )
      else
        InputState.Choose(
          undo = None,
          acc = Accumulated((actor = game.turn), List()),
          select = eventTypeSelect(
            actorItems = actorItems,
            opponentItems = opponentItems,
            shotgun = game.shotgun,
          ),
        )
    InputButtonsComponent.render(initial, result => submit.event(result))
  }

  private val LiveChoice = ChoiceElement.of(Live, "Live")
  private val BlankChoice = ChoiceElement.of(Blank, "Blank")

  private val PlayerChoice = ChoiceElement.of(Player, "Player")
  private val DealerChoice = ChoiceElement.of(Dealer, "Dealer")

  private enum EventType:
    case Shot, Used

  private type Shot = EventType.Shot.type
  private type Used = EventType.Used.type

  private val ShotChoice = ChoiceElement.of(EventType.Shot, "shot")
  private val UsedChoice = ChoiceElement.of(EventType.Used, "used")

  private val AdrenalineChoice = ChoiceElement.of(Adrenaline, "Adrenaline")
  private val HandcuffsChoice = ChoiceElement.of(Handcuffs, "Handcuffs")
  private val MagnifyingGlassChoice = ChoiceElement.of(MagnifyingGlass, "Magnifying Glass")
  private val BeerChoice = ChoiceElement.of(Beer, "Beer")
  private val CigarettesChoice = ChoiceElement.of(Cigarettes, "Cigarettes")
  private val SawChoice = ChoiceElement.of(Saw, "Saw")
  private val InverterChoice = ChoiceElement.of(Inverter, "Inverter")
  private val BurnerPhoneChoice = ChoiceElement.of(BurnerPhone, "Burner Phone")
  private val MedsChoice = ChoiceElement.of(Meds, "Meds")

  private enum MedsQuality:
    case Good, Bad

  private val GoodMedsChoice = ChoiceElement.of(MedsQuality.Good, "Good")
  private val BadMedsChoice = ChoiceElement.of(MedsQuality.Bad, "Bad")

  private val NoShellChoice = ChoiceElement.of(None, "nothing")
  private val Shell2Choice = ChoiceElement.of(Shell2, "2d shell")
  private val Shell3Choice = ChoiceElement.of(Shell3, "3d shell")
  private val Shell4Choice = ChoiceElement.of(Shell4, "4th shell")
  private val Shell5Choice = ChoiceElement.of(Shell5, "5th shell")
  private val Shell6Choice = ChoiceElement.of(Shell6, "6th shell")
  private val Shell7Choice = ChoiceElement.of(Shell7, "7th shell")
  private val Shell8Choice = ChoiceElement.of(Shell8, "8th shell")

  private val ShotShellSelect =
    shellSelect[Actor ++ Target, Event.DealerShot | Event.PlayerShot](description = Label("with")) { (state, shell) =>
      state.actor match
        case Player => Event.PlayerShot(target = state.target, shell = shell)
        case Dealer => Event.DealerShot(target = state.target, shell = shell)
    }

  private def shellSelect[State, E <: Event](description: Label)(builder: AccBuilder[State, Shell, E]) =
    Select(
      description = description.some,
      options = Seq(LiveChoice, BlankChoice).map { choice =>
        choice
          .whenState[State]
          .onClickReady(builder)
      },
    )

  private def magnifyingGlassShellSelect(stolen: Boolean) =
    shellSelect[Unit, Event.PlayerUsed](description = Label("and revealed")) { (state, shell) =>
      Event.PlayerUsed(FullItemUse.MagnifyingGlass(shell), stolen)
    }

  private def phoneShellSelect(stolen: Boolean) =
    shellSelect[Position, Event.PlayerUsed](description = Label("as")) { (state, shell) =>
      Event.PlayerUsed(FullItemUse.BurnerPhone((revealed = shell, at = state.at).some), stolen)
    }

  private def medsQualitySelect(stolen: Boolean) = Select[MedsQuality, Actor, Event.PlayerUsed | Event.DealerUsed](
    description = Label("and they were").some,
    options = Seq(GoodMedsChoice, BadMedsChoice).map { choice =>
      choice
        .whenState[Actor]
        .onClickReady { (state, quality) =>
          val good = quality == MedsQuality.Good
          itemUseEvent(state, stolen)(
            FullItemUse.Meds(good),
            ItemUse.Meds(good),
          )
        }
    },
  )

  private def beerShellSelect(stolen: Boolean) =
    shellSelect[Actor, Event.PlayerUsed | Event.DealerUsed](description = Label("and revealed")) { (state, shell) =>
      itemUseEvent(state, stolen)(
        FullItemUse.Beer(shell),
        ItemUse.Beer(shell),
      )
    }

  private val TargetSelect = Select[Side, Actor, Event.DealerShot | Event.PlayerShot](
    description = None,
    options = Seq(PlayerChoice, DealerChoice).map { choice =>
      choice
        .whenState[Actor]
        .onClickNext(ShotShellSelect)((state, target) => state ++ (target = target))
    },
  )

  private def phonePositionSelect(shotgun: Shotgun, stolen: Boolean) =
    Select[SeqNr | None.type, Unit, Event.PlayerUsed](
      description = Label("and revealed").some,
      options = {
        val nothing =
          NoShellChoice
            .whenState[Unit]
            .onClickReady[Event.PlayerUsed] { (_, nothing) =>
              Event.PlayerUsed(FullItemUse.BurnerPhone(nothing), stolen)
            }

        val positions =
          (shotgun.total minus Nat[1])
            .map: available =>
              Seq(Shell2Choice, Shell3Choice, Shell4Choice, Shell5Choice, Shell6Choice, Shell7Choice, Shell8Choice)
                .take(available)
            .getOrElse(Seq.empty)

        nothing +: positions.map: choice =>
          choice
            .whenState[Unit]
            .onClickNext(phoneShellSelect(stolen)) { (_, seqNr) =>
              (at = seqNr)
            }
      },
    )

  private def stealItemSelect(opponentItems: Items, shotgun: Shotgun) =
    Select[RegularItem, Actor, Event.PlayerUsed | Event.DealerUsed](
      description = Label("to steal").some,
      options = opponentItems.getRegular.toVector.sortBy(_.toString).map {
        case Handcuffs =>
          HandcuffsChoice
            .whenState[Actor]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = true)(
                FullItemUse.Handcuffs,
                ItemUse.Handcuffs,
              )
            }
        case MagnifyingGlass =>
          MagnifyingGlassChoice
            .whenState[Actor]
            .onClick { state =>
              state.actor match
                case Player => Requires.next(magnifyingGlassShellSelect(stolen = true))(())
                case Dealer => Requires.Ready(Event.DealerUsed(item = ItemUse.MagnifyingGlass, stolen = true))
            }
        case Beer =>
          BeerChoice
            .whenState[Actor]
            .onClickNext(beerShellSelect(stolen = true))(keepState)
        case Cigarettes =>
          CigarettesChoice
            .whenState[Actor]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = true)(
                FullItemUse.Cigarettes,
                ItemUse.Cigarettes,
              )
            }
        case Saw =>
          SawChoice
            .whenState[Actor]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = true)(
                FullItemUse.Saw,
                ItemUse.Saw,
              )
            }
        case Inverter =>
          InverterChoice
            .whenState[Actor]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = true)(
                FullItemUse.Inverter,
                ItemUse.Inverter,
              )
            }
        case BurnerPhone =>
          BurnerPhoneChoice
            .whenState[Actor]
            .onClick { state =>
              state.actor match
                case Player => Requires.next(phonePositionSelect(shotgun, stolen = true))(())
                case Dealer => Requires.Ready(Event.DealerUsed(item = ItemUse.BurnerPhone, stolen = true))
            }
        case Meds =>
          MedsChoice
            .whenState[Actor]
            .onClickNext(medsQualitySelect(stolen = true))(keepState)
      },
    )

  private def itemSelect(actorItems: Items, opponentItems: Items, shotgun: Shotgun) =
    Select[Item, Actor, Event.PlayerUsed | Event.DealerUsed](
      description = None,
      options = actorItems.asSet.toVector.sortBy(_.toString).map {
        case Adrenaline =>
          AdrenalineChoice
            .whenState[Actor]
            .onClickNext(stealItemSelect(opponentItems, shotgun))(keepState)
        case Handcuffs =>
          HandcuffsChoice
            .whenState[Actor]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = false)(
                FullItemUse.Handcuffs,
                ItemUse.Handcuffs,
              )
            }
        case MagnifyingGlass =>
          MagnifyingGlassChoice
            .whenState[Actor]
            .onClick { state =>
              state.actor match
                case Player => Requires.next(magnifyingGlassShellSelect(stolen = false))(())
                case Dealer => Requires.Ready(Event.DealerUsed(item = ItemUse.MagnifyingGlass, stolen = false))
            }
        case Beer =>
          BeerChoice
            .whenState[Actor]
            .onClickNext(beerShellSelect(stolen = false))(keepState)
        case Cigarettes =>
          CigarettesChoice
            .whenState[Actor]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = false)(
                FullItemUse.Cigarettes,
                ItemUse.Cigarettes,
              )
            }
        case Saw =>
          SawChoice
            .whenState[Actor]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = false)(
                FullItemUse.Saw,
                ItemUse.Saw,
              )
            }
        case Inverter =>
          InverterChoice
            .whenState[Actor]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = false)(
                FullItemUse.Inverter,
                ItemUse.Inverter,
              )
            }
        case BurnerPhone =>
          BurnerPhoneChoice
            .whenState[Actor]
            .onClick { state =>
              state.actor match
                case Player => Requires.next(phonePositionSelect(shotgun, stolen = false))(())
                case Dealer => Requires.Ready(Event.DealerUsed(item = ItemUse.BurnerPhone, stolen = false))
            }
        case Meds =>
          MedsChoice
            .whenState[Actor]
            .onClickNext(medsQualitySelect(stolen = false))(keepState)
      },
    )

  private def eventTypeSelect(actorItems: Items, opponentItems: Items, shotgun: Shotgun) =
    Select[EventType, Actor, Event](
      description = None,
      options = Seq(
        ShotChoice
          .whenState[Actor]
          .onClickNext(TargetSelect)(keepState),
        UsedChoice
          .whenState[Actor]
          .onClickNext(itemSelect(actorItems, opponentItems, shotgun))(keepState),
      ),
    )

  private type Actor = (actor: Side)
  private type Target = (target: Side)
  private type Position = (at: SeqNr)

  private infix type ++[A <: AnyNamedTuple, B <: AnyNamedTuple] = Concat[A, B]

  private type ActorTarget = Actor ++ Target

  private def itemUseEvent(state: Actor, stolen: Boolean)(
    player: => FullItemUse,
    dealer: => ItemUse,
  ): Event.PlayerUsed | Event.DealerUsed =
    state.actor match
      case Player => Event.PlayerUsed(item = player, stolen = stolen)
      case Dealer => Event.DealerUsed(item = dealer, stolen = stolen)

  private def keepState[A]: AccBuilder[A, Any, A] = (state, _) => state
