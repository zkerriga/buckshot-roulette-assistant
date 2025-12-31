package com.zkerriga.buckshot.tui

import cats.syntax.all.*
import com.googlecode.lanterna.gui2.{Component, Label, Panel}
import com.zkerriga.buckshot.engine
import com.zkerriga.buckshot.game.events.Used.ItemUse
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state.items.Item

object InputComponent2:
  enum Event:
    case DealerShot(target: Side, shell: Shell)
    case PlayerShot(target: Side, shell: Shell)
    case DealerUsed(item: ItemUse, stolen: Boolean)
    case PlayerUsed(item: engine.events.PlayerUsed.ItemUse, stolen: Boolean)

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

  private val ShellSelect =
    shellSelect[ActorShotTarget, Event.DealerShot | Event.PlayerShot](description = Label("with"))

  private def shellSelect[State, E <: Event](
    description: Label,
  )(using AccBuilder[State, Live.type, E], AccBuilder[State, Blank.type, E]) = Select[Shell, State, E](
    description = description.some,
    options = Seq(
      LiveChoice
        .whenState[State]
        .onClickFinalizeTo[E],
      BlankChoice
        .whenState[State]
        .onClickFinalizeTo[E],
    ),
  )

  private val GlassShellSelect =
    shellSelect[PlayerUsedMagnifyingGlass, Event.PlayerUsed](description = Label("and revealed"))

  private val MedsQualitySelect = Select[MedsQuality, ActorUsedMeds, Event.PlayerUsed | Event.DealerUsed](
    description = Label("and they were").some,
    options = Seq(
      GoodMedsChoice
        .whenState[ActorUsedMeds]
        .onClickFinalizeTo[Event.PlayerUsed | Event.DealerUsed],
      BadMedsChoice
        .whenState[ActorUsedMeds]
        .onClickFinalizeTo[Event.PlayerUsed | Event.DealerUsed],
    ),
  )

  private val BeerShellSelect =
    shellSelect[ActorUsedBeer, Event.PlayerUsed | Event.DealerUsed](description = Label("and revealed"))

  private val TargetSelect = Select[Side, ActorShot, Event.DealerShot | Event.PlayerShot](
    description = None,
    options = Seq(
      PlayerChoice
        .whenState[ActorShot]
        .onClickContinueTo(ShellSelect),
      DealerChoice
        .whenState[ActorShot]
        .onClickContinueTo(ShellSelect),
    ),
  )

  private def itemSelect(items: Items) = Select[Item, ActorUsed, Event.PlayerUsed | Event.DealerUsed](
    description = None,
    options = items.asSet.toVector.sortBy(_.toString).map {
      case Adrenaline =>
        ???
      case Handcuffs =>
        HandcuffsChoice
          .whenState[ActorUsed]
          .onClickFinalizeTo[Event.PlayerUsed | Event.DealerUsed]
      case MagnifyingGlass =>
        MagnifyingGlassChoice
          .whenState[ActorUsed]
          .onClick { acc =>
            acc.actor match
              case Player =>
                Requires.next(GlassShellSelect)(actor = Player, event = acc.event, item = MagnifyingGlass)
              case Dealer => Requires.Ready(Event.DealerUsed(item = ItemUse.MagnifyingGlass, stolen = false))
          }
      case Beer =>
        BeerChoice
          .whenState[ActorUsed]
          .onClickContinueTo(BeerShellSelect)
      case Cigarettes =>
        CigarettesChoice
          .whenState[ActorUsed]
          .onClickFinalizeTo[Event.PlayerUsed | Event.DealerUsed]
      case Saw =>
        SawChoice
          .whenState[ActorUsed]
          .onClickFinalizeTo[Event.PlayerUsed | Event.DealerUsed]
      case Inverter =>
        InverterChoice
          .whenState[ActorUsed]
          .onClickFinalizeTo[Event.PlayerUsed | Event.DealerUsed]
      case BurnerPhone => ???
      case Meds =>
        MedsChoice
          .whenState[ActorUsed]
          .onClickContinueTo(MedsQualitySelect)
    },
  )

  private type Actor = (actor: Side)

  private type ActorShot = (actor: Side, event: Shot)
  private given AccBuilder[Actor, Shot, ActorShot] = (before, value) => before ++ (event = value)

  private type ActorShotTarget = (actor: Side, event: Shot, target: Side)
  private given [S <: Side]: AccBuilder[ActorShot, S, ActorShotTarget] = (before, value) => before ++ (target = value)

  private given [S <: Shell]: AccBuilder[ActorShotTarget, S, Event.DealerShot | Event.PlayerShot] = (before, value) =>
    before.actor match
      case Player => Event.PlayerShot(target = before.target, shell = value)
      case Dealer => Event.DealerShot(target = before.target, shell = value)

  private type ActorUsed = (actor: Side, event: Used)
  private given AccBuilder[Actor, Used, ActorUsed] = (before, value) => before ++ (event = value)

  private type ActorUsedItem[I <: Item] = (actor: Side, event: Used, item: I)
  private type ActorUsedBeer = ActorUsedItem[Beer.type]
  private given [S <: Shell]: AccBuilder[ActorUsedBeer, S, Event.PlayerUsed | Event.DealerUsed] = (before, shell) =>
    before.actor match
      case Player => Event.PlayerUsed(item = engine.events.PlayerUsed.ItemUse.Beer(shell), stolen = false)
      case Dealer => Event.DealerUsed(item = ItemUse.Beer(shell), stolen = false)

  private type ActorUsedMeds = ActorUsedItem[Meds.type]
  private given [Q <: MedsQuality]: AccBuilder[ActorUsedMeds, Q, Event.PlayerUsed | Event.DealerUsed] =
    (before, quality) =>
      val good = quality == MedsQuality.Good
      before.actor match
        case Player => Event.PlayerUsed(item = engine.events.PlayerUsed.ItemUse.Meds(good), stolen = false)
        case Dealer => Event.DealerUsed(item = ItemUse.Meds(good), stolen = false)

  private type PlayerUsedMagnifyingGlass = (actor: Player.type, event: Used, item: MagnifyingGlass.type)
  private given [S <: Shell]: AccBuilder[PlayerUsedMagnifyingGlass, S, Event.PlayerUsed] = (before, shell) =>
    Event.PlayerUsed(item = engine.events.PlayerUsed.ItemUse.MagnifyingGlass(shell), stolen = false)

  private given AccBuilder[ActorUsed, Handcuffs.type, Event.PlayerUsed | Event.DealerUsed] = (before, value) =>
    before.actor match
      case Player => Event.PlayerUsed(item = engine.events.PlayerUsed.ItemUse.Handcuffs, stolen = false)
      case Dealer => Event.DealerUsed(item = ItemUse.Handcuffs, stolen = false)

  private given AccBuilder[ActorUsed, Cigarettes.type, Event.PlayerUsed | Event.DealerUsed] = (before, value) =>
    before.actor match
      case Player => Event.PlayerUsed(item = engine.events.PlayerUsed.ItemUse.Cigarettes, stolen = false)
      case Dealer => Event.DealerUsed(item = ItemUse.Cigarettes, stolen = false)

  private given AccBuilder[ActorUsed, Saw.type, Event.PlayerUsed | Event.DealerUsed] = (before, value) =>
    before.actor match
      case Player => Event.PlayerUsed(item = engine.events.PlayerUsed.ItemUse.Saw, stolen = false)
      case Dealer => Event.DealerUsed(item = ItemUse.Saw, stolen = false)

  private given AccBuilder[ActorUsed, Inverter.type, Event.PlayerUsed | Event.DealerUsed] = (before, value) =>
    before.actor match
      case Player => Event.PlayerUsed(item = engine.events.PlayerUsed.ItemUse.Inverter, stolen = false)
      case Dealer => Event.DealerUsed(item = ItemUse.Inverter, stolen = false)

  private given AccBuilder[ActorUsed, Beer.type, ActorUsedBeer] = (before, value) =>
    (actor = before.actor, event = before.event, item = value)

  private given AccBuilder[ActorUsed, Meds.type, ActorUsedMeds] = (before, value) =>
    (actor = before.actor, event = before.event, item = value)

  private def eventTypeSelect(items: Items) = Select[EventType, Actor, Event](
    description = None,
    options = Seq[Choice[EventType, Actor, Event]](
      ShotChoice
        .whenState[Actor]
        .onClickContinueTo(TargetSelect),
      UsedChoice
        .whenState[Actor]
        .onClickContinueTo(itemSelect(items)),
    ),
  )
