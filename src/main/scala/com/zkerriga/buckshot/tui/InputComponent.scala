package com.zkerriga.buckshot.tui

import cats.syntax.all.*
import com.googlecode.lanterna.gui2.{Component, Label}
import com.zkerriga.buckshot.engine.events.PlayerUsed.ItemUse as FullItemUse
import com.zkerriga.buckshot.game.accessors.Opposition
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Used.ItemUse
import com.zkerriga.buckshot.game.state.items.Item
import com.zkerriga.buckshot.tui.InputButtonsComponent.SelectPrefix.EmptySelectPrefix
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

    val initial: InputState[Event] =
      if cannotUseAnyItem then
        InputState(
          undo = None,
          acc = Accumulated(Seq(EmptySelectPrefix -> ShotChoice)),
          next = Requires.NextChoice(targetSelect(game.turn)),
        )
      else
        InputState(
          undo = None,
          acc = Accumulated(Seq.empty),
          next = Requires.NextChoice(
            eventTypeSelect(
              actor = game.turn,
              actorItems = actorItems,
              opponentItems = opponentItems,
              shotgun = game.shotgun,
            ),
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

  private def shotShellSelect(actor: Side, target: Side) =
    SelectPrefix("with").withOptions:
      shellChoice[Event.DealerShot | Event.PlayerShot] { shell =>
        actor match
          case Player => Event.PlayerShot(target = target, shell = shell)
          case Dealer => Event.DealerShot(target = target, shell = shell)
      }

  private def shellChoice[E <: Event](builder: Shell => E) =
    Seq(LiveChoice, BlankChoice).map { choice =>
      choice.onClickReady(builder)
    }

  private def magnifyingGlassShellSelect(stolen: Boolean) =
    SelectPrefix("and revealed").withOptions:
      shellChoice[Event.PlayerUsed] { shell =>
        Event.PlayerUsed(FullItemUse.MagnifyingGlass(shell), stolen)
      }

  private def phoneShellSelect(at: SeqNr, stolen: Boolean) =
    SelectPrefix("as").withOptions:
      shellChoice[Event.PlayerUsed] { shell =>
        Event.PlayerUsed(FullItemUse.BurnerPhone((revealed = shell, at = at).some), stolen)
      }

  private def medsQualitySelect(actor: Side, stolen: Boolean) =
    SelectPrefix("and they were").withOptions[MedsQuality, Event.PlayerUsed | Event.DealerUsed]:
      Seq(GoodMedsChoice, BadMedsChoice).map { choice =>
        choice.onClickReady { quality =>
          val good = quality == MedsQuality.Good
          itemUseEvent(actor, stolen)(
            FullItemUse.Meds(good),
            ItemUse.Meds(good),
          )
        }
      }

  private def beerShellSelect(actor: Side, stolen: Boolean) =
    SelectPrefix("and revealed").withOptions:
      shellChoice[Event.PlayerUsed | Event.DealerUsed] { shell =>
        itemUseEvent(actor, stolen)(
          FullItemUse.Beer(shell),
          ItemUse.Beer(shell),
        )
      }

  private def targetSelect(actor: Side) =
    EmptySelectPrefix.withOptions[Side, Event.DealerShot | Event.PlayerShot]:
      Seq(PlayerChoice, DealerChoice).map { choice =>
        choice.onClickNext(shotShellSelect(actor, choice.value))
      }

  private def phonePositionSelect(shotgun: Shotgun, stolen: Boolean) =
    SelectPrefix("and revealed").withOptions[SeqNr | None.type, Event.PlayerUsed] {
      val nothing =
        NoShellChoice
          .onClickReady[Event.PlayerUsed] { nothing =>
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
          .onClickNext(phoneShellSelect(choice.value, stolen))
    }

  private def stealItemSelect(actor: Side, opponentItems: Items, shotgun: Shotgun) =
    SelectPrefix("to steal").withOptions[RegularItem, Event.PlayerUsed | Event.DealerUsed]:
      opponentItems.getRegular.toVector.sortBy(_.toString).map {
        case Handcuffs =>
          HandcuffsChoice.onClickReady { _ =>
            itemUseEvent(actor, stolen = true)(
              FullItemUse.Handcuffs,
              ItemUse.Handcuffs,
            )
          }
        case MagnifyingGlass =>
          MagnifyingGlassChoice.onClick {
            actor match
              case Player => Requires.NextChoice(magnifyingGlassShellSelect(stolen = true))
              case Dealer => Requires.Ready(Event.DealerUsed(item = ItemUse.MagnifyingGlass, stolen = true))
          }
        case Beer =>
          BeerChoice.onClickNext(beerShellSelect(actor, stolen = true))
        case Cigarettes =>
          CigarettesChoice.onClickReady { _ =>
            itemUseEvent(actor, stolen = true)(
              FullItemUse.Cigarettes,
              ItemUse.Cigarettes,
            )
          }
        case Saw =>
          SawChoice.onClickReady { _ =>
            itemUseEvent(actor, stolen = true)(
              FullItemUse.Saw,
              ItemUse.Saw,
            )
          }
        case Inverter =>
          InverterChoice.onClickReady { _ =>
            itemUseEvent(actor, stolen = true)(
              FullItemUse.Inverter,
              ItemUse.Inverter,
            )
          }
        case BurnerPhone =>
          BurnerPhoneChoice.onClick {
            actor match
              case Player => Requires.NextChoice(phonePositionSelect(shotgun, stolen = true))
              case Dealer => Requires.Ready(Event.DealerUsed(item = ItemUse.BurnerPhone, stolen = true))
          }
        case Meds =>
          MedsChoice.onClickNext(medsQualitySelect(actor, stolen = true))
      }

  private def itemSelect(actor: Side, actorItems: Items, opponentItems: Items, shotgun: Shotgun) =
    EmptySelectPrefix.withOptions[Item, Event.PlayerUsed | Event.DealerUsed]:
      actorItems.asSet.toVector.sortBy(_.toString).map {
        case Adrenaline =>
          AdrenalineChoice.onClickNext(stealItemSelect(actor, opponentItems, shotgun))
        case Handcuffs =>
          HandcuffsChoice.onClickReady { _ =>
            itemUseEvent(actor, stolen = false)(
              FullItemUse.Handcuffs,
              ItemUse.Handcuffs,
            )
          }
        case MagnifyingGlass =>
          MagnifyingGlassChoice.onClick {
            actor match
              case Player => Requires.NextChoice(magnifyingGlassShellSelect(stolen = false))
              case Dealer => Requires.Ready(Event.DealerUsed(item = ItemUse.MagnifyingGlass, stolen = false))
          }
        case Beer =>
          BeerChoice.onClickNext(beerShellSelect(actor, stolen = false))
        case Cigarettes =>
          CigarettesChoice.onClickReady { _ =>
            itemUseEvent(actor, stolen = false)(
              FullItemUse.Cigarettes,
              ItemUse.Cigarettes,
            )
          }
        case Saw =>
          SawChoice.onClickReady { _ =>
            itemUseEvent(actor, stolen = false)(
              FullItemUse.Saw,
              ItemUse.Saw,
            )
          }
        case Inverter =>
          InverterChoice.onClickReady { _ =>
            itemUseEvent(actor, stolen = false)(
              FullItemUse.Inverter,
              ItemUse.Inverter,
            )
          }
        case BurnerPhone =>
          BurnerPhoneChoice.onClick {
            actor match
              case Player => Requires.NextChoice(phonePositionSelect(shotgun, stolen = false))
              case Dealer => Requires.Ready(Event.DealerUsed(item = ItemUse.BurnerPhone, stolen = false))
          }
        case Meds =>
          MedsChoice.onClickNext(medsQualitySelect(actor, stolen = false))
      }

  private def eventTypeSelect(actor: Side, actorItems: Items, opponentItems: Items, shotgun: Shotgun) =
    EmptySelectPrefix.withOptions:
      Seq(
        ShotChoice.onClickNext(targetSelect(actor)),
        UsedChoice.onClickNext(itemSelect(actor, actorItems, opponentItems, shotgun)),
      )

  private def itemUseEvent(actor: Side, stolen: Boolean)(
    player: => FullItemUse,
    dealer: => ItemUse,
  ): Event.PlayerUsed | Event.DealerUsed =
    actor match
      case Player => Event.PlayerUsed(item = player, stolen = stolen)
      case Dealer => Event.DealerUsed(item = dealer, stolen = stolen)
