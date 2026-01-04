package com.zkerriga.buckshot.tui

import cats.syntax.all.*
import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.BeliefState
import com.zkerriga.buckshot.engine.state.{GameState, Revealed}
import com.zkerriga.buckshot.game
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state
import com.zkerriga.buckshot.game.state.shotgun.Shell
import com.zkerriga.buckshot.tui.ItemGridComponent.labelName
import com.zkerriga.types.Chance

object GameStateComponent:
  def render(game: GameState): Component =
    gameState(game).withBorder(Borders.singleLine())

  private def gameState(game: GameState): Panel =
    import LinearLayout.*
    Panel(LinearLayout(Direction.VERTICAL)).withAll(
      dealer(game).setLayoutData(createLayoutData(Alignment.Fill)),
      Separator(Direction.HORIZONTAL).setLayoutData(createLayoutData(Alignment.Fill)),
      EmptySpace(),
      shotgun(game.shotgun),
      EmptySpace(),
      Separator(Direction.HORIZONTAL).setLayoutData(createLayoutData(Alignment.Fill)),
      player(game).setLayoutData(createLayoutData(Alignment.Fill)),
    )

  private def dealer(game: GameState): Panel =
    import LinearLayout.*
    Panel(LinearLayout(Direction.VERTICAL)).withAll(
      participantTitle(Dealer, game.turn),
      Separator(Direction.HORIZONTAL).setLayoutData(createLayoutData(Alignment.Fill)),
      EmptySpace(),
      items(game.dealer.items),
      EmptySpace(),
      meta(game.dealer, game.hidden.dealer.belief, game.shotgun, game.maxHealth)
        .setLayoutData(createLayoutData(Alignment.Fill)),
    )

  private def player(game: GameState): Panel =
    import LinearLayout.*
    Panel(LinearLayout(Direction.VERTICAL)).withAll(
      meta(game.player, BeliefState.deterministic(game.hidden.player.revealed), game.shotgun, game.maxHealth)
        .setLayoutData(createLayoutData(Alignment.Fill)),
      EmptySpace(),
      items(game.player.items),
      EmptySpace(),
      Separator(Direction.HORIZONTAL).setLayoutData(createLayoutData(Alignment.Fill)),
      participantTitle(Player, game.turn),
    )

  private def participantTitle(side: Side, turnOf: Side): Panel =
    Panel(GridLayout(2)).withSeq:
      Seq(
        Label(side.toString).some,
        Option.when(side == turnOf)(Label("← turn")),
      ).flatten

  private def items(items: Items): Component =
    ItemGridComponent.render(items) {
      case Some(itemOn) => Label(itemOn.item.labelName)
      case None => Label(" ")
    }

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

  private def meta(participant: Participant, known: BeliefState[Revealed], shotgun: Shotgun, max: HealthLimit): Panel =
    import GridLayout.*
    Panel(GridLayout(4)).withAll(
      revealed(known, shotgun).setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
      EmptySpace().setLayoutData(createHorizontallyFilledLayoutData()),
      hands(participant.hands).setLayoutData(createLayoutData(Alignment.END, Alignment.CENTER)),
      health(participant.health, max).setLayoutData(createLayoutData(Alignment.END, Alignment.CENTER)),
    )

  private def hands(hands: Hands): Label =
    hands match
      case Hands.Free => Label("")
      case Hands.CuffedForTwoShots => Label("Cuffed (2)")
      case Hands.CuffedForOneShot => Label("Cuffed (1)")

  private def revealed(states: BeliefState[Revealed], shotgun: Shotgun): Panel =
    val shellsToShow = Seq(Shell1, Shell2, Shell3, Shell4, Shell5, Shell6, Shell7, Shell8).take(shotgun.total)
    Panel(LinearLayout(Direction.VERTICAL)).withSeq:
      states.asSortedSeq.map: (chance, revealed) =>
        Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
          Label("Knows"),
          shells(shellsToShow.map(revealed.get)),
          ChanceLabel.renderUncertain(chance),
        )

  private def health(current: Health, limit: HealthLimit): Label =
    Label(("☇" * current.asInt).padTo(limit.asInt, '-').reverse)

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
