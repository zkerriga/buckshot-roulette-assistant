package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.gui2.*
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

    panel.addComponent(participantTitle(Dealer, game.turnOf))
    panel.addComponent(items(game.dealer.items).withBorder(Borders.singleLine()))
    panel.addComponent(meta(game.dealer, game.shotgun, game.maxHealth))
    panel.addComponent(shotgun(game.shotgun))
    panel.addComponent(meta(game.player, game.shotgun, game.maxHealth))
    panel.addComponent(items(game.player.items).withBorder(Borders.singleLine()))
    panel.addComponent(participantTitle(Player, game.turnOf))

    panel

  def participantTitle(side: Side, turnOf: Side): Panel =
    val panel = Panel(LinearLayout(Direction.HORIZONTAL))

    panel.addComponent(Label(side.toString))
    if side == turnOf then panel.addComponent(Label("← turn"))

    panel

  def items(items: Items): Panel =
    val panel = Panel(LinearLayout(Direction.VERTICAL))

    items.asList
      .sortBy(_.toString)
      .map:
        case Adrenaline => "Adrenaline"
        case Handcuffs => "Handcuffs"
        case MagnifyingGlass => "Magnifying Glass"
        case Beer => "Beer"
        case Cigarettes => "Cigarettes"
        case Saw => "Saw"
        case Inverter => "Inverter"
        case BurnerPhone => "Burner Phone"
        case Meds => "Meds"
      .foreach: item =>
        panel.addComponent(Label(item))

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

  def meta(participant: Participant, shotgun: Shotgun, maxHealth: HealthLimit): Panel =
    val panel = Panel(LinearLayout(Direction.HORIZONTAL))

    panel.addComponent(revealed(participant.revealed, shotgun))
    panel.addComponent(hands(participant.hands))
    panel.addComponent(health(participant.health, maxHealth))

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
    Label(("⚡" * current).padTo(limit, '-').reverse)

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
