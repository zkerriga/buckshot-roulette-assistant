package com.zkerriga.buckshot.tui

import cats.syntax.all.*
import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.game
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state
import com.zkerriga.buckshot.game.state.shotgun.Shell
import com.zkerriga.buckshot.game.state.{GameState, partitipant}

object GameStateComponent:
  def render(game: GameState): Component =
    gameState(game).withBorder(Borders.singleLine())

  private def gameState(game: GameState): Panel =
    import LinearLayout.*
    Panel(LinearLayout(Direction.VERTICAL)).withAll(
      participant(game, Dealer).setLayoutData(createLayoutData(Alignment.Fill)),
      Separator(Direction.HORIZONTAL).setLayoutData(createLayoutData(Alignment.Fill)),
      EmptySpace(),
      shotgun(game.shotgun),
      EmptySpace(),
      Separator(Direction.HORIZONTAL).setLayoutData(createLayoutData(Alignment.Fill)),
      participant(game, Player).setLayoutData(createLayoutData(Alignment.Fill)),
    )

  private def participant(game: GameState, side: Side): Panel =
    def topToBottom: Seq[Component] => Seq[Component] = identity
    def bottomToTop: Seq[Component] => Seq[Component] = _.reverse
    val (participant, order) = side match
      case Player => (game.player, bottomToTop)
      case Dealer => (game.dealer, topToBottom)

    import LinearLayout.*
    Panel(LinearLayout(Direction.VERTICAL)).withSeq:
      order:
        Seq(
          participantTitle(side, game.turnOf),
          Separator(Direction.HORIZONTAL).setLayoutData(createLayoutData(Alignment.Fill)),
          EmptySpace(),
          items(participant.items),
          EmptySpace(),
          meta(participant, game.shotgun, game.maxHealth).setLayoutData(createLayoutData(Alignment.Fill)),
        )

  private def participantTitle(side: Side, turnOf: Side): Panel =
    Panel(GridLayout(2)).withSeq:
      Seq(
        Label(side.toString).some,
        Option.when(side == turnOf)(Label("← turn")),
      ).flatten

  private def items(items: Items): Panel =
    Panel(GridLayout(3)).withSeq:
      val labels = items.asList
        .sortBy(_.toString)
        .map:
          case Adrenaline => Label("Adrenaline") // todo: colors can be set
          case Handcuffs => Label("Handcuffs")
          case MagnifyingGlass => Label("Magnifying Glass")
          case Beer => Label("Beer")
          case Cigarettes => Label("Cigarettes")
          case Saw => Label("Saw")
          case Inverter => Label("Inverter")
          case BurnerPhone => Label("Burner Phone")
          case Meds => Label("Meds")
      val (left, right) = (labels ++ Seq.fill(8 - labels.size)(Label("-"))).splitAt(4)
      left
        .zip(right)
        .flatMap: (leftItem, rightItem) =>
          Seq(
            leftItem,
            Separator(Direction.VERTICAL),
            rightItem,
          )

  private def shotgun(shotgun: Shotgun): Panel =
    Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
      shotgunShells(shotgun.shells),
      EmptySpace(),
      shotgunEffects(shotgun.effects),
    )

  private def shells(shells: Seq[Option[Shell]]): Panel =
    Panel(LinearLayout(Direction.HORIZONTAL)).withSeq:
      shells
        .map:
          case Some(Live) => Label("L").setBackgroundColor(TextColor.ANSI.RED)
          case Some(Blank) => Label("B").setBackgroundColor(TextColor.ANSI.BLUE)
          case None => Label("?").setBackgroundColor(TextColor.ANSI.BLACK_BRIGHT)

  private def meta(participant: Participant, shotgun: Shotgun, maxHealth: HealthLimit): Panel =
    import GridLayout.*
    Panel(GridLayout(4)).withAll(
      revealed(participant.revealed, shotgun).setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
      EmptySpace().setLayoutData(createHorizontallyFilledLayoutData()),
      hands(participant.hands).setLayoutData(createLayoutData(Alignment.END, Alignment.CENTER)),
      health(participant.health, maxHealth).setLayoutData(createLayoutData(Alignment.END, Alignment.CENTER)),
    )

  private def hands(hands: Hands): Label =
    hands match
      case Hands.Free => Label("")
      case Hands.CuffedForTwoShots => Label("Cuffed (2)")
      case Hands.CuffedForOneShot => Label("Cuffed (1)")

  private def revealed(revealed: Revealed, shotgun: Shotgun): Panel =
    val shellSequence =
      Seq(Shell1, Shell2, Shell3, Shell4, Shell5, Shell6, Shell7, Shell8)
        .take(shotgun.total)
        .map(revealed.get)
    Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
      Label("Knows"),
      shells(shellSequence),
    )

  private def health(current: Health, limit: HealthLimit): Label =
    Label(("☇" * current).padTo(limit, '-').reverse)

  private def shotgunShells(shotgun: Shotgun.ShellDistribution): Panel =
    Panel(GridLayout(2)).withAll(
      Label("Live"),
      shells(List.fill(shotgun.live)(Some(Live))),
      Label("Blank"),
      shells(List.fill(shotgun.blank)(Some(Blank))),
    )

  private def shotgunEffects(shotgun: Shotgun.Effects): Panel =
    Panel(LinearLayout(Direction.VERTICAL)).withSeq:
      Seq(
        Option.when(shotgun.damage == Damage.Double)(Label("Damage x2")),
        Option.when(shotgun.inverted)(Label("Inverted")),
      ).flatten
