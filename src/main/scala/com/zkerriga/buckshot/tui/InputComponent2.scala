package com.zkerriga.buckshot.tui

import cats.syntax.all.*
import com.googlecode.lanterna.gui2.{Component, Label}
import com.zkerriga.buckshot.engine.events.PlayerUsed.ItemUse as FullItemUse
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Used.ItemUse
import com.zkerriga.buckshot.game.state.items.Item
import com.zkerriga.types.Nat

object InputComponent2:
  enum Event:
    case DealerShot(target: Side, shell: Shell)
    case PlayerShot(target: Side, shell: Shell)
    case DealerUsed(item: ItemUse, stolen: Boolean)
    case PlayerUsed(item: FullItemUse, stolen: Boolean)

  trait Submit:
    def event(event: Event): Unit

  import InputButtonsComponent.*

  def render(game: TableState, submit: Submit): Component = {
    val initial: InputState.Choose[?, ?, Event] = ???
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

  private val ShotChoice = ChoiceElement.of(EventType.Shot, "Shot")
  private val UsedChoice = ChoiceElement.of(EventType.Used, "Used")

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
    shellSelect[ActorShotTarget, Event.DealerShot | Event.PlayerShot](description = Label("with")) { (state, shell) =>
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

  private val GlassShellSelect =
    shellSelect[PlayerUsedMagnifyingGlass, Event.PlayerUsed](description = Label("and revealed")) { (state, shell) =>
      Event.PlayerUsed(item = FullItemUse.MagnifyingGlass(shell), stolen = false)
    }

  private val PhoneShellSelect =
    shellSelect[PlayerUsedBurnerPhonePosition, Event.PlayerUsed](description = Label("as")) { (state, shell) =>
      Event.PlayerUsed(
        item = FullItemUse.BurnerPhone((revealed = shell, at = state.at).some),
        stolen = false,
      )
    }

  private val MedsQualitySelect = Select[MedsQuality, ActorUsedMeds, Event.PlayerUsed | Event.DealerUsed](
    description = Label("and they were").some,
    options = Seq(GoodMedsChoice, BadMedsChoice).map { choice =>
      choice
        .whenState[ActorUsedMeds]
        .onClickReady { (state, quality) =>
          val good = quality == MedsQuality.Good
          itemUseEvent(state.init, stolen = false)(
            FullItemUse.Meds(good),
            ItemUse.Meds(good),
          )
        }
    },
  )

  private val BeerShellSelect =
    shellSelect[ActorUsedBeer, Event.PlayerUsed | Event.DealerUsed](description = Label("and revealed")) {
      (state, shell) =>
        itemUseEvent(state.init, stolen = false)(
          FullItemUse.Beer(shell),
          ItemUse.Beer(shell),
        )
    }

  private val TargetSelect = Select[Side, ActorShot, Event.DealerShot | Event.PlayerShot](
    description = None,
    options = Seq(PlayerChoice, DealerChoice).map { choice =>
      choice
        .whenState[ActorShot]
        .onClickNext(ShotShellSelect)((state, target) => state ++ (target = target))
    },
  )

  private def shellPositionSelect(shotgun: Shotgun) =
    Select[SeqNr | None.type, PlayerUsedBurnerPhone, Event.PlayerUsed](
      description = Label("and revealed").some,
      options = {
        val nothing =
          NoShellChoice
            .whenState[PlayerUsedBurnerPhone]
            .onClickReady[Event.PlayerUsed] { (state, _) =>
              Event.PlayerUsed(FullItemUse.BurnerPhone(None), stolen = false)
            }

        val positions =
          (shotgun.total minus Nat[1])
            .map: available =>
              Seq(Shell2Choice, Shell3Choice, Shell4Choice, Shell5Choice, Shell6Choice, Shell7Choice, Shell8Choice)
                .take(available)
            .getOrElse(Seq.empty)

        nothing +: positions.map: choice =>
          choice
            .whenState[PlayerUsedBurnerPhone]
            .onClickNext(PhoneShellSelect) { (state, seqNr) =>
              state ++ (at = seqNr)
            }
      },
    )

  private def stealItemSelect(opponentItems: Items, shotgun: Shotgun) =
    Select[RegularItem, ActorUsed, Event.PlayerUsed | Event.DealerUsed](
      description = Label("to steal").some,
      options = opponentItems.getRegular.toVector.sortBy(_.toString).map {
        case Handcuffs =>
          HandcuffsChoice
            .whenState[ActorUsed]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = true)(
                FullItemUse.Handcuffs,
                ItemUse.Handcuffs,
              )
            }
        case MagnifyingGlass => ???
        case Beer => ???
        case Cigarettes =>
          CigarettesChoice
            .whenState[ActorUsed]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = true)(
                FullItemUse.Cigarettes,
                ItemUse.Cigarettes,
              )
            }
        case Saw =>
          SawChoice
            .whenState[ActorUsed]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = true)(
                FullItemUse.Saw,
                ItemUse.Saw,
              )
            }
        case Inverter =>
          InverterChoice
            .whenState[ActorUsed]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = true)(
                FullItemUse.Inverter,
                ItemUse.Inverter,
              )
            }
        case BurnerPhone => ???
        case Meds => ???
      },
    )

  private def itemSelect(actorItems: Items, opponentItems: Items, shotgun: Shotgun) =
    Select[Item, ActorUsed, Event.PlayerUsed | Event.DealerUsed](
      description = None,
      options = actorItems.asSet.toVector.sortBy(_.toString).map {
        case Adrenaline =>
          AdrenalineChoice
            .whenState[ActorUsed]
            .onClickNext(stealItemSelect(opponentItems, shotgun))((state, _) => state)
        case Handcuffs =>
          HandcuffsChoice
            .whenState[ActorUsed]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = false)(
                FullItemUse.Handcuffs,
                ItemUse.Handcuffs,
              )
            }
        case MagnifyingGlass =>
          MagnifyingGlassChoice
            .whenState[ActorUsed]
            .onClick { acc =>
              acc.actor match
                case Player =>
                  Requires.next(GlassShellSelect)(actor = Player, event = acc.event, item = MagnifyingGlass)
                case Dealer =>
                  Requires.Ready(Event.DealerUsed(item = ItemUse.MagnifyingGlass, stolen = false))
            }
        case Beer =>
          BeerChoice
            .whenState[ActorUsed]
            .onClickNext(BeerShellSelect) { (state, beer) =>
              (actor = state.actor, event = state.event, item = beer)
            }
        case Cigarettes =>
          CigarettesChoice
            .whenState[ActorUsed]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = false)(
                FullItemUse.Cigarettes,
                ItemUse.Cigarettes,
              )
            }
        case Saw =>
          SawChoice
            .whenState[ActorUsed]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = false)(
                FullItemUse.Saw,
                ItemUse.Saw,
              )
            }
        case Inverter =>
          InverterChoice
            .whenState[ActorUsed]
            .onClickReady { (state, _) =>
              itemUseEvent(state, stolen = false)(
                FullItemUse.Inverter,
                ItemUse.Inverter,
              )
            }
        case BurnerPhone =>
          BurnerPhoneChoice
            .whenState[ActorUsed]
            .onClick { acc =>
              acc.actor match
                case Player =>
                  Requires.next(shellPositionSelect(shotgun))((actor = Player, event = acc.event, item = BurnerPhone))
                case Dealer =>
                  Requires.Ready(Event.DealerUsed(item = ItemUse.BurnerPhone, stolen = false))
            }
        case Meds =>
          MedsChoice
            .whenState[ActorUsed]
            .onClickNext(MedsQualitySelect) { (state, meds) =>
              (actor = state.actor, event = state.event, item = meds)
            }
      },
    )

  private type Actor = (actor: Side)

  private type ActorShot = (actor: Side, event: Shot)
  private type ActorShotTarget = (actor: Side, event: Shot, target: Side)

  private type ActorUsed = (actor: Side, event: Used)
  private type ActorUsedItem[I <: Item] = (actor: Side, event: Used, item: I)
  private type ActorUsedBeer = ActorUsedItem[Beer.type]
  private type ActorUsedMeds = ActorUsedItem[Meds.type]
  private type PlayerUsedMagnifyingGlass = (actor: Player.type, event: Used, item: MagnifyingGlass.type)
  private type PlayerUsedBurnerPhone = (actor: Player.type, event: Used, item: BurnerPhone.type)
  private type PlayerUsedBurnerPhonePosition = (actor: Player.type, event: Used, item: BurnerPhone.type, at: SeqNr)

  private def itemUseEvent(state: (actor: Side, event: Used), stolen: Boolean)(
    player: => FullItemUse,
    dealer: => ItemUse,
  ): Event.PlayerUsed | Event.DealerUsed =
    state.actor match
      case Player => Event.PlayerUsed(item = player, stolen = stolen)
      case Dealer => Event.DealerUsed(item = dealer, stolen = stolen)

  private def eventTypeSelect(actorItems: Items, opponentItems: Items, shotgun: Shotgun) =
    Select[EventType, Actor, Event](
      description = None,
      options = Seq(
        ShotChoice
          .whenState[Actor]
          .onClickNext(TargetSelect) { (state, shot) =>
            (actor = state.actor, event = shot)
          },
        UsedChoice
          .whenState[Actor]
          .onClickNext(itemSelect(actorItems, opponentItems, shotgun)) { (state, used) =>
            (actor = state.actor, event = used)
          },
      ),
    )
