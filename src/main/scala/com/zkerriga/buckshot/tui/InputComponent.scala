package com.zkerriga.buckshot.tui

import cats.syntax.all.*
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.gui2.Component
import com.zkerriga.buckshot.engine.events.{PlayerUsed, PlayerShot, DealerShot, DealerUsed, FullItemUse}
import com.zkerriga.buckshot.engine.Engine.Event
import com.zkerriga.buckshot.game.accessors.Opposition
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.ItemUse
import com.zkerriga.types.Nat
import com.zkerriga.types.Opt.syntax.given

object InputComponent:
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
      actorItems.positioned.isEmpty || (actorItems.containAdrenaline && opponentItems.positioned.isEmpty)

    val initial: InputState[Event] =
      if cannotUseAnyItem then
        InputState(
          undo = None,
          acc = Accumulated(Seq(SelectPrefix() -> ShotChoice)),
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

  private val LiveChoice = ChoiceElement.of(Live, "Live", back = ANSI.RED)
  private val BlankChoice = ChoiceElement.of(Blank, "Blank", back = ANSI.BLUE)

  private val PlayerChoice = ChoiceElement.of(Player, "Player", front = ANSI.MAGENTA)
  private val DealerChoice = ChoiceElement.of(Dealer, "Dealer", front = ANSI.MAGENTA)

  private enum EventType:
    case Shot, Used

  private val ShotChoice = ChoiceElement.of(EventType.Shot, "shot")
  private val UsedChoice = ChoiceElement.of(EventType.Used, "used")

  private val AdrenalineChoice = ChoiceElement.of(Adrenaline, "Adrenaline", front = ANSI.GREEN)
  private val HandcuffsChoice = ChoiceElement.of(Handcuffs, "Handcuffs", front = ANSI.YELLOW)
  private val MagnifyingGlassChoice = ChoiceElement.of(MagnifyingGlass, "Magnifying Glass", front = ANSI.YELLOW)
  private val BeerChoice = ChoiceElement.of(Beer, "Beer", front = ANSI.YELLOW)
  private val CigarettesChoice = ChoiceElement.of(Cigarettes, "Cigarettes", front = ANSI.YELLOW)
  private val SawChoice = ChoiceElement.of(Saw, "Saw", front = ANSI.YELLOW)
  private val InverterChoice = ChoiceElement.of(Inverter, "Inverter", front = ANSI.YELLOW)
  private val BurnerPhoneChoice = ChoiceElement.of(BurnerPhone, "Burner Phone", front = ANSI.YELLOW)
  private val MedsChoice = ChoiceElement.of(Meds, "Meds", front = ANSI.YELLOW)

  private enum MedsQuality:
    case Good, Bad

  private val GoodMedsChoice = ChoiceElement.of(MedsQuality.Good, "Good", front = ANSI.CYAN)
  private val BadMedsChoice = ChoiceElement.of(MedsQuality.Bad, "Bad", front = ANSI.CYAN)

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
      shellChoice[DealerShot | PlayerShot] { shell =>
        actor match
          case Player => PlayerShot(target = target, shell = shell)
          case Dealer => DealerShot(target = target, shell = shell)
      }

  private def shellChoice[E <: Event](builder: Shell => E) =
    Seq(LiveChoice, BlankChoice).map { choice =>
      choice.onClickReady(builder)
    }

  private def magnifyingGlassShellSelect(on: Slot, viaAdrenaline: Option[Slot]) =
    SelectPrefix("and revealed").withOptions:
      shellChoice[PlayerUsed] { shell =>
        PlayerUsed(FullItemUse.MagnifyingGlass(shell), on = on, viaAdrenaline = viaAdrenaline)
      }

  private def phoneShellSelect(at: SeqNr, on: Slot, viaAdrenaline: Option[Slot]) =
    SelectPrefix("as").withOptions:
      shellChoice[PlayerUsed] { shell =>
        PlayerUsed(FullItemUse.BurnerPhone((revealed = shell, at = at).some), on = on, viaAdrenaline = viaAdrenaline)
      }

  private def medsQualitySelect(actor: Side, on: Slot, viaAdrenaline: Option[Slot]) =
    SelectPrefix("and they were").withOptions[MedsQuality, PlayerUsed | DealerUsed]:
      Seq(GoodMedsChoice, BadMedsChoice).map { choice =>
        choice.onClickReady { quality =>
          val good = quality == MedsQuality.Good
          itemUseEvent(actor, on, viaAdrenaline)(
            FullItemUse.Meds(good),
            ItemUse.Meds(good),
          )
        }
      }

  private def beerShellSelect(actor: Side, on: Slot, viaAdrenaline: Option[Slot]) =
    SelectPrefix("and revealed").withOptions:
      shellChoice[PlayerUsed | DealerUsed] { shell =>
        itemUseEvent(actor, on, viaAdrenaline)(
          FullItemUse.Beer(shell),
          ItemUse.Beer(shell),
        )
      }

  private def targetSelect(actor: Side) =
    SelectPrefix().withOptions[Side, DealerShot | PlayerShot]:
      Seq(PlayerChoice, DealerChoice).map { choice =>
        choice.onClickNext(shotShellSelect(actor, choice.value))
      }

  private def phonePositionSelect(shotgun: Shotgun, on: Slot, viaAdrenaline: Option[Slot]) =
    SelectPrefix("and revealed").withOptions[SeqNr | None.type, PlayerUsed] {
      val nothing =
        NoShellChoice.onClickReady[PlayerUsed] { nothing =>
          PlayerUsed(FullItemUse.BurnerPhone(nothing), on = on, viaAdrenaline = viaAdrenaline)
        }

      val positions =
        (shotgun.total minus Nat[1])
          .map: available =>
            Seq(Shell2Choice, Shell3Choice, Shell4Choice, Shell5Choice, Shell6Choice, Shell7Choice, Shell8Choice)
              .take(available)
          .getOrElse(Seq.empty)

      nothing +: positions.map: choice =>
        choice.onClickNext(phoneShellSelect(choice.value, on = on, viaAdrenaline = viaAdrenaline))
    }

  private def stealItemSelect(actor: Side, items: Items, shotgun: Shotgun, adrenaline: Slot) =
    SelectPrefix("to steal").withItems(items) { (item, slot) =>
      item match {
        case Adrenaline => None
        case Handcuffs =>
          HandcuffsChoice.onClickReady { _ =>
            itemUseEvent(actor, on = slot, viaAdrenaline = adrenaline)(
              FullItemUse.Handcuffs,
              ItemUse.Handcuffs,
            )
          }
        case MagnifyingGlass =>
          MagnifyingGlassChoice.onClick {
            actor match
              case Player => Requires.NextChoice(magnifyingGlassShellSelect(on = slot, viaAdrenaline = adrenaline))
              case Dealer =>
                Requires.Ready(DealerUsed(item = ItemUse.MagnifyingGlass, on = slot, viaAdrenaline = adrenaline))
          }
        case Beer =>
          BeerChoice.onClickNext(beerShellSelect(actor, on = slot, viaAdrenaline = adrenaline))
        case Cigarettes =>
          CigarettesChoice.onClickReady { _ =>
            itemUseEvent(actor, on = slot, viaAdrenaline = adrenaline)(
              FullItemUse.Cigarettes,
              ItemUse.Cigarettes,
            )
          }
        case Saw =>
          SawChoice.onClickReady { _ =>
            itemUseEvent(actor, on = slot, viaAdrenaline = adrenaline)(
              FullItemUse.Saw,
              ItemUse.Saw,
            )
          }
        case Inverter =>
          InverterChoice.onClickReady { _ =>
            itemUseEvent(actor, on = slot, viaAdrenaline = adrenaline)(
              FullItemUse.Inverter,
              ItemUse.Inverter,
            )
          }
        case BurnerPhone =>
          BurnerPhoneChoice.onClick {
            actor match
              case Player => Requires.NextChoice(phonePositionSelect(shotgun, on = slot, viaAdrenaline = adrenaline))
              case Dealer =>
                Requires.Ready(DealerUsed(item = ItemUse.BurnerPhone, on = slot, viaAdrenaline = adrenaline))
          }
        case Meds =>
          MedsChoice.onClickNext(medsQualitySelect(actor, on = slot, viaAdrenaline = adrenaline))
      }
    }

  private def itemSelect(actor: Side, actorItems: Items, opponentItems: Items, shotgun: Shotgun) =
    SelectPrefix().withItems(actorItems) { (item, slot) =>
      item match {
        case Adrenaline =>
          AdrenalineChoice.onClickNext(stealItemSelect(actor, opponentItems, shotgun, slot))
        case Handcuffs =>
          HandcuffsChoice.onClickReady { _ =>
            itemUseEvent(actor, on = slot, viaAdrenaline = None)(
              FullItemUse.Handcuffs,
              ItemUse.Handcuffs,
            )
          }
        case MagnifyingGlass =>
          MagnifyingGlassChoice.onClick {
            actor match
              case Player => Requires.NextChoice(magnifyingGlassShellSelect(on = slot, viaAdrenaline = None))
              case Dealer =>
                Requires.Ready(DealerUsed(item = ItemUse.MagnifyingGlass, on = slot, viaAdrenaline = None))
          }
        case Beer =>
          BeerChoice.onClickNext(beerShellSelect(actor, on = slot, viaAdrenaline = None))
        case Cigarettes =>
          CigarettesChoice.onClickReady { _ =>
            itemUseEvent(actor, on = slot, viaAdrenaline = None)(
              FullItemUse.Cigarettes,
              ItemUse.Cigarettes,
            )
          }
        case Saw =>
          SawChoice.onClickReady { _ =>
            itemUseEvent(actor, on = slot, viaAdrenaline = None)(
              FullItemUse.Saw,
              ItemUse.Saw,
            )
          }
        case Inverter =>
          InverterChoice.onClickReady { _ =>
            itemUseEvent(actor, on = slot, viaAdrenaline = None)(
              FullItemUse.Inverter,
              ItemUse.Inverter,
            )
          }
        case BurnerPhone =>
          BurnerPhoneChoice.onClick {
            actor match
              case Player => Requires.NextChoice(phonePositionSelect(shotgun, on = slot, viaAdrenaline = None))
              case Dealer => Requires.Ready(DealerUsed(item = ItemUse.BurnerPhone, on = slot, viaAdrenaline = None))
          }
        case Meds =>
          MedsChoice.onClickNext(medsQualitySelect(actor, on = slot, viaAdrenaline = None))
      }
    }

  private def eventTypeSelect(actor: Side, actorItems: Items, opponentItems: Items, shotgun: Shotgun) =
    SelectPrefix().withOptions:
      Seq(
        ShotChoice.onClickNext(targetSelect(actor)),
        UsedChoice.onClickNext(itemSelect(actor, actorItems, opponentItems, shotgun)),
      )

  private def itemUseEvent(actor: Side, on: Slot, viaAdrenaline: Option[Slot])(
    player: => FullItemUse,
    dealer: => ItemUse,
  ): PlayerUsed | DealerUsed =
    actor match
      case Player => PlayerUsed(item = player, on = on, viaAdrenaline = viaAdrenaline)
      case Dealer => DealerUsed(item = dealer, on = on, viaAdrenaline = viaAdrenaline)
