package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.game
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state
import com.zkerriga.buckshot.game.state.partitipant
import com.zkerriga.buckshot.game.state.shotgun.Shell

object Render:
  def window(game: GameState): Window =
    val window = BasicWindow("Buckshot Roulette Assistant")
    val content = Panel(LinearLayout(Direction.VERTICAL))
    content.addComponent(composition(game))
    content.addComponent(quit(window), LinearLayout.createLayoutData(LinearLayout.Alignment.End))
    window.setComponent(content)
    window

  def quit(window: Window): Button =
    Button(
      "Quit",
      () => window.close(),
    )

  def composition(game: GameState): Panel =
    val panel = Panel(LinearLayout(Direction.HORIZONTAL))

    panel.addComponent(gameState(game).withBorder(Borders.singleLine()))
    panel.addComponent(EmptySpace())
    panel.addComponent(control())

    panel

  def control(): Panel =
    val panel = Panel(LinearLayout(Direction.VERTICAL))

    panel.addComponent(Label("Input event:"))
    panel.addComponent(TextBox())

    panel

  def gameState(game: GameState): Panel =
    val panel = Panel(LinearLayout(Direction.VERTICAL))

    panel.addComponent(
      participant(game, Dealer).setLayoutData(
        LinearLayout.createLayoutData(LinearLayout.Alignment.Fill),
      ),
    )
    panel.addComponent(
      Separator(Direction.HORIZONTAL).setLayoutData(
        LinearLayout.createLayoutData(LinearLayout.Alignment.Fill),
      ),
    )
    panel.addComponent(EmptySpace())
    panel.addComponent(shotgun(game.shotgun))
    panel.addComponent(EmptySpace())
    panel.addComponent(
      Separator(Direction.HORIZONTAL).setLayoutData(
        LinearLayout.createLayoutData(LinearLayout.Alignment.Fill),
      ),
    )
    panel.addComponent(
      participant(game, Player).setLayoutData(
        LinearLayout.createLayoutData(LinearLayout.Alignment.Fill),
      ),
    )

    panel

  def participant(game: GameState, side: Side): Panel =
    val panel = Panel(LinearLayout(Direction.VERTICAL))

    def topToBottom: Seq[Component] => Seq[Component] = identity
    def bottomToTop: Seq[Component] => Seq[Component] = _.reverse

    val (participant, order) = side match
      case Player => (game.player, bottomToTop)
      case Dealer => (game.dealer, topToBottom)

    order(
      Seq(
        participantTitle(side, game.turnOf),
        Separator(Direction.HORIZONTAL).setLayoutData(
          LinearLayout.createLayoutData(LinearLayout.Alignment.Fill),
        ),
        EmptySpace(),
        items(participant.items),
        EmptySpace(),
        meta(participant, game.shotgun, game.maxHealth).setLayoutData(
          LinearLayout.createLayoutData(LinearLayout.Alignment.Fill),
        ),
      ),
    ).foreach(panel.addComponent)

    panel

  def participantTitle(side: Side, turnOf: Side): Panel =
    val panel = Panel(GridLayout(2))

    panel.addComponent(Label(side.toString))
    if side == turnOf then panel.addComponent(Label("← turn"))

    panel

  def items(items: Items): Panel =
    val panel = Panel(GridLayout(3))

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

    val (left, right) = (labels ++ List.fill(8 - labels.size)(Label("-"))).splitAt(4)
    left
      .zip(right)
      .foreach: (leftItem, rightItem) =>
        panel.addComponent(leftItem)
        panel.addComponent(Separator(Direction.VERTICAL))
        panel.addComponent(rightItem)

    panel

  def shotgun(shotgun: Shotgun): Panel =
    val panel = Panel(LinearLayout(Direction.HORIZONTAL))

    panel.addComponent(shotgunShells(shotgun.shells))
    panel.addComponent(EmptySpace())
    panel.addComponent(shotgunEffects(shotgun.effects))

    panel

  def shells(shells: List[Option[Shell]]): Panel =
    val panel = Panel(LinearLayout(Direction.HORIZONTAL))

    shells
      .map:
        case Some(Live) => Label("L").setBackgroundColor(TextColor.ANSI.RED)
        case Some(Blank) => Label("B").setBackgroundColor(TextColor.ANSI.BLUE)
        case None => Label("?").setBackgroundColor(TextColor.ANSI.BLACK_BRIGHT)
      .foreach: label =>
        panel.addComponent(label)

    panel

  private def gridLayout(
    horizontalAlignment: GridLayout.Alignment,
    verticalAlignment: GridLayout.Alignment,
    grabExtraHorizontalSpace: Boolean,
    grabExtraVerticalSpace: Boolean,
  ): LayoutData = GridLayout.createLayoutData(
    horizontalAlignment,
    verticalAlignment,
    grabExtraHorizontalSpace,
    grabExtraVerticalSpace,
  )

  def meta(participant: Participant, shotgun: Shotgun, maxHealth: HealthLimit): Panel =
    val panel = Panel(GridLayout(4))

    panel.addComponent(
      revealed(participant.revealed, shotgun).setLayoutData(
        gridLayout(
          horizontalAlignment = GridLayout.Alignment.BEGINNING,
          verticalAlignment = GridLayout.Alignment.CENTER,
          grabExtraHorizontalSpace = false,
          grabExtraVerticalSpace = false,
        ),
      ),
    )
    panel.addComponent(EmptySpace().setLayoutData(GridLayout.createHorizontallyFilledLayoutData()))
    panel.addComponent(
      hands(participant.hands).setLayoutData(
        gridLayout(
          horizontalAlignment = GridLayout.Alignment.END,
          verticalAlignment = GridLayout.Alignment.CENTER,
          grabExtraHorizontalSpace = false,
          grabExtraVerticalSpace = false,
        ),
      ),
    )
    panel.addComponent(
      health(participant.health, maxHealth).setLayoutData(
        gridLayout(
          horizontalAlignment = GridLayout.Alignment.END,
          verticalAlignment = GridLayout.Alignment.CENTER,
          grabExtraHorizontalSpace = false,
          grabExtraVerticalSpace = false,
        ),
      ),
    )

    panel

  def hands(hands: Hands): Label =
    hands match
      case Hands.Free => Label("")
      case Hands.CuffedForTwoShots => Label("Cuffed (2)")
      case Hands.CuffedForOneShot => Label("Cuffed (1)")

  def revealed(revealed: Revealed, shotgun: Shotgun): Panel =
    val panel = Panel(LinearLayout(Direction.HORIZONTAL))

    val shellSequence =
      List(Shell1, Shell2, Shell3, Shell4, Shell5, Shell6, Shell7, Shell8)
        .take(shotgun.total)
        .map(revealed.get)

    panel.addComponent(Label("Knows"))
    panel.addComponent(shells(shellSequence))

    panel

  def health(current: Health, limit: HealthLimit): Label =
    Label(("☇" * current).padTo(limit, '-').reverse)

  def shotgunShells(shotgun: Shotgun.ShellDistribution): Panel =
    val panel = Panel(GridLayout(2))

    panel.addComponent(Label("Live"))
    panel.addComponent(shells(List.fill(shotgun.live)(Some(Live))))
    panel.addComponent(Label("Blank"))
    panel.addComponent(shells(List.fill(shotgun.blank)(Some(Blank))))

    panel

  def shotgunEffects(shotgun: Shotgun.Effects): Panel =
    val panel = Panel(LinearLayout(Direction.VERTICAL))

    shotgun.damage match
      case Damage.Single => ()
      case Damage.Double => panel.addComponent(Label("Damage x2"))

    if shotgun.inverted then panel.addComponent(Label("Inverted"))

    panel
