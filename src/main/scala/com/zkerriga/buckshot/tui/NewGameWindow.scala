package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.engine.BeliefState
import com.zkerriga.buckshot.engine.state.PrivateStates.{DealerKnowledge, DealerNotes, PlayerKnowledge}
import com.zkerriga.buckshot.engine.state.{GameState, PrivateStates, Revealed}
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.types.Nat

object NewGameWindow {
  trait Submit:
    def state(state: GameState): Unit

  def window(submit: Submit): Window =
    val window = BasicWindow("New Game")
    val (compositionComponent, formData) = composition()
    val startButton = Button(
      "Start",
      () =>
        formData.get().foreach { data =>
          val table = TableState(
            maxHealth = data.limit,
            turn = Player,
            dealer = Participant(
              health = data.limit.maxAllowed,
              items = data.dealer,
              hands = Hands.Free,
            ),
            shotgun = Shotgun.fresh(live = data.live, blank = data.blank),
            player = Participant(
              health = data.limit.maxAllowed,
              items = data.player,
              hands = Hands.Free,
            ),
          )
          val state = GameState(
            public = table,
            hidden = PrivateStates(
              dealer = DealerKnowledge(
                belief = BeliefState.deterministic(Revealed.Nothing),
                notes = DealerNotes(
                  usedMeds = false,
                  slotGroups = List(data.dealer.positioned.map(_.on).toSet),
                ),
              ),
              player = PlayerKnowledge(
                revealed = Revealed.Nothing,
              ),
            ),
          )
          submit.state(state)
        },
    )
    import LinearLayout.*
    val content = Panel(LinearLayout(Direction.VERTICAL)).withAll(
      compositionComponent,
      startButton.setLayoutData(createLayoutData(Alignment.End)),
      QuitButton.render(window).setLayoutData(createLayoutData(Alignment.End)),
    )
    window

  private trait Get[A]:
    def get(): A
  private object Get:
    def nullable[A](get: => A): Get[Option[A]] = () => Option(get)
    def of[A](get: => A): Get[A] = () => get

  private def composition() = {
    val (healthLimitComponent, healthLimit) = healthLimitForm()
    val (liveShellsComponent, liveShells) = shellsForm()
    val (blankShellsComponent, blankShells) = shellsForm()
    val (dealerItemsComponent, dealerItems) = itemsForm()
    val (playerItemsComponent, playerItems) = itemsForm()

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
        healthLimit <- healthLimit.get()
        liveShells <- liveShells.get()
        blankShells <- blankShells.get()
      } yield (
        limit = healthLimit,
        live = liveShells,
        blank = blankShells,
        dealer = dealerItems.get(),
        player = playerItems.get(),
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

  private def itemsForm(): (Component, Get[Items]) =
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

    ItemGridComponent.render(Items.Empty) { (optItem, slot) =>
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
            case (Some(item: Item), slot) => (item, slot)
      }
    }
}
