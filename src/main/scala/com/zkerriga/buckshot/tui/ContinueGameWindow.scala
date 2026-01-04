package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.BeliefState
import com.zkerriga.buckshot.engine.events.ContinuableOutcome
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerKnowledge, DealerNotes, PlayerKnowledge}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.types.Nat

object ContinueGameWindow {
  trait Submit:
    def state(state: GameState): Unit

  def window(outcome: ContinuableOutcome, submit: Submit): Window =
    val window = BasicWindow("Continue Game")
    val (compositionComponent, gameState) =
      outcome match {
        case reset: ContinuableOutcome.ResetDetails => onReset(reset)
        case win: ContinuableOutcome.WinDetails => onWin(win)
      }
    val startButton = Button(
      "Start",
      () =>
        gameState.get().foreach { state =>
          submit.state(state)
          window.close()
        },
    )
    import LinearLayout.*
    val content = Panel(LinearLayout(Direction.VERTICAL)).withAll(
      compositionComponent,
      startButton.setLayoutData(createLayoutData(Alignment.End)),
      QuitButton.render(window).setLayoutData(createLayoutData(Alignment.End)),
    )
    window.setComponent(content)
    window

  private trait Get[A]:
    def get(): A
  private object Get:
    def nullable[A](get: => A): Get[Option[A]] = () => Option(get)
    def of[A](get: => A): Get[A] = () => get

  private def onReset(reset: ContinuableOutcome.ResetDetails): (Component, Get[Option[GameState]]) = {
    val (liveShellsComponent, liveShells) = shellsForm()
    val (blankShellsComponent, blankShells) = shellsForm()
    val (dealerItemsComponent, getDealerItems) = itemsForm(reset.reset.dealer.items)
    val (playerItemsComponent, getPlayerItems) = itemsForm(reset.reset.player.items)

    import GridLayout.*
    def separation() =
      Seq.fill(2)(Separator(Direction.HORIZONTAL).setLayoutData(createHorizontallyFilledLayoutData()))

    Panel(GridLayout(2)).withSeq(
      Seq(
        Seq(
          Panel(LinearLayout(Direction.HORIZONTAL))
            .withAll(
              Label("Shotgun"),
              Label("Live").setBackgroundColor(TextColor.ANSI.RED),
            )
            .setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          liveShellsComponent.withBorder(Borders.singleLine()),
        ),
        Seq(
          Panel(LinearLayout(Direction.HORIZONTAL))
            .withAll(
              Label("Shotgun"),
              Label("Blank").setBackgroundColor(TextColor.ANSI.BLUE),
            )
            .setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          blankShellsComponent.withBorder(Borders.singleLine()),
        ),
        separation(),
        Seq(
          Label("Dealer Items").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          dealerItemsComponent,
        ),
        separation(),
        Seq(
          Label("Player Items").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          playerItemsComponent,
        ),
      ).flatten,
    ) -> Get.of {
      for {
        liveShells <- liveShells.get()
        blankShells <- blankShells.get()
        dealerItems = getDealerItems.get()
        playerItems = getPlayerItems.get()
      } yield GameState(
        public = TableState(
          maxHealth = reset.reset.maxHealth,
          turn = Player,
          dealer = Participant(
            health = reset.reset.dealer.health,
            items = dealerItems,
            hands = Hands.Free,
          ),
          shotgun = Shotgun.fresh(live = liveShells, blank = blankShells),
          player = Participant(
            health = reset.reset.player.health,
            items = {
              val oldItemSlots = reset.reset.player.items.positioned.map(_.on).toSet
              val newItems = playerItems.positioned.filterNot(itemOn => oldItemSlots.contains(itemOn.on))
              Items(
                adrenaline = playerItems.adrenaline,
                positioned = reset.reset.player.items.positioned :++ newItems,
              )
            },
            hands = Hands.Free,
          ),
        ),
        hidden = PrivateStates(
          dealer = DealerKnowledge(
            belief = BeliefState.deterministic(Revealed.Nothing),
            notes = DealerNotes(
              usedMeds = false,
              slotGroups = {
                val oldItemSlots = reset.dealerItemGroups.foldLeft(Set.empty[Slot])(_ ++ _)
                val newItems = dealerItems.positioned.map(_.on).toSet -- oldItemSlots
                (reset.dealerItemGroups :+ newItems).filter(_.nonEmpty)
              },
            ),
          ),
          player = PlayerKnowledge(
            revealed = Revealed.Nothing,
          ),
        ),
      )
    }
  }

  private def onWin(win: ContinuableOutcome.WinDetails): (Component, Get[Option[GameState]]) = {
    val (healthLimitComponent, healthLimit) = healthLimitForm()
    val (liveShellsComponent, liveShells) = shellsForm()
    val (blankShellsComponent, blankShells) = shellsForm()
    val (dealerItemsComponent, getDealerItems) = itemsForm(win.win.dealer)
    val (playerItemsComponent, getPlayerItems) = itemsForm(win.win.player)

    import GridLayout.*
    def separation() =
      Seq.fill(2)(Separator(Direction.HORIZONTAL).setLayoutData(createHorizontallyFilledLayoutData()))

    Panel(GridLayout(2)).withSeq(
      Seq(
        Seq(
          Label("Starting Health").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          healthLimitComponent.withBorder(Borders.singleLine()),
        ),
        separation(),
        Seq(
          Panel(LinearLayout(Direction.HORIZONTAL))
            .withAll(
              Label("Shotgun"),
              Label("Live").setBackgroundColor(TextColor.ANSI.RED),
            )
            .setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          liveShellsComponent.withBorder(Borders.singleLine()),
        ),
        Seq(
          Panel(LinearLayout(Direction.HORIZONTAL))
            .withAll(
              Label("Shotgun"),
              Label("Blank").setBackgroundColor(TextColor.ANSI.BLUE),
            )
            .setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          blankShellsComponent.withBorder(Borders.singleLine()),
        ),
        separation(),
        Seq(
          Label("Dealer Items").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          dealerItemsComponent,
        ),
        separation(),
        Seq(
          Label("Player Items").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          playerItemsComponent,
        ),
      ).flatten,
    ) -> Get.of {
      for {
        maxHealth <- healthLimit.get()
        liveShells <- liveShells.get()
        blankShells <- blankShells.get()
        dealerItems = getDealerItems.get()
        playerItems = getPlayerItems.get()
      } yield GameState(
        public = TableState(
          maxHealth = maxHealth,
          turn = Player,
          dealer = Participant(
            health = maxHealth.maxAllowed,
            items = dealerItems,
            hands = Hands.Free,
          ),
          shotgun = Shotgun.fresh(live = liveShells, blank = blankShells),
          player = Participant(
            health = maxHealth.maxAllowed,
            items = {
              val oldItemSlots = win.win.player.positioned.map(_.on).toSet
              val newItems = playerItems.positioned.filterNot(itemOn => oldItemSlots.contains(itemOn.on))
              Items(
                adrenaline = playerItems.adrenaline,
                positioned = win.win.player.positioned :++ newItems,
              )
            },
            hands = Hands.Free,
          ),
        ),
        hidden = PrivateStates(
          dealer = DealerKnowledge(
            belief = BeliefState.deterministic(Revealed.Nothing),
            notes = DealerNotes(
              usedMeds = false,
              slotGroups = {
                val oldItemSlots = win.dealerItemGroups.foldLeft(Set.empty[Slot])(_ ++ _)
                val newItems = dealerItems.positioned.map(_.on).toSet -- oldItemSlots
                (win.dealerItemGroups :+ newItems).filter(_.nonEmpty)
              },
            ),
          ),
          player = PlayerKnowledge(
            revealed = Revealed.Nothing,
          ),
        ),
      )
    }
  }

  private def healthLimitForm(): (Component, Get[Option[HealthLimit]]) =
    val available: Seq[HealthLimit] = Seq(
      HealthLimit[2],
      HealthLimit[3],
      HealthLimit[4],
      HealthLimit[5],
    )
    val box = ComboBox[HealthLimit](available*)
    box.setSelectedItem(HealthLimit[5])
    box -> Get.nullable(box.getSelectedItem)

  private def shellsForm(): (Component, Get[Option[Nat]]) =
    val box = ComboBox[Nat](
      Nat[1],
      Nat[2],
      Nat[3],
      Nat[4],
    )
    box -> Get.nullable(box.getSelectedItem)

  private def itemsForm(exiting: Items): (Component, Get[Items]) =
    val items: Seq[Item | "-"] = "-" +: (RegularItem.values.toSeq :+ Adrenaline).sortBy(_.toString)

    def box(): ComboBox[Item | "-"] = ComboBox(items*)

    val box1 = box()
    val box2 = box()
    val box3 = box()
    val box4 = box()
    val box5 = box()
    val box6 = box()
    val box7 = box()
    val box8 = box()

    ItemGridComponent.render(exiting) { (optItem, slot) =>
      val box = slot match {
        case Slot1 => box1
        case Slot2 => box2
        case Slot3 => box3
        case Slot4 => box4
        case Slot5 => box5
        case Slot6 => box6
        case Slot7 => box7
        case Slot8 => box8
      }
      optItem.foreach { item =>
        box.clearItems()
        box.addItem(item)
        box.setSelectedItem(item)
      }
      box
    } -> Get.of {
      Items.from {
        Seq(box1, box2, box3, box4, box5, box6, box7, box8)
          .map(box => Option(box.getSelectedItem))
          .zip(Slot.values)
          .collect:
            case (Some(item: Item), slot) => slot -> item
      }
    }
}
