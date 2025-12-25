package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.game
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state
import com.zkerriga.buckshot.game.state.items.{Item, RegularItem}
import com.zkerriga.buckshot.game.state.partitipant
import com.zkerriga.types.Nat

object SetupWindow:
  def window(setState: GameState => Unit): Window =
    val window = BasicWindow("New Game")
    val (compositionPanel, getState) = composition()
    val startButton = Button(
      "Start",
      () =>
        getState().state match
          case Some(state) =>
            setState(state)
            window.close()
          case None => (), // todo: log error
    )
    import LinearLayout.*
    val content = Panel(LinearLayout(Direction.VERTICAL)).withAll(
      compositionPanel,
      startButton.setLayoutData(createLayoutData(Alignment.End)),
      QuitButton.render(window).setLayoutData(createLayoutData(Alignment.End)),
    )
    window.setComponent(content)
    window

  private case class StateForm(state: Option[GameState])
  private def composition(): (Panel, () => StateForm) =
    val (dealerHealthPanel, getDealerHealth, updateDealerHealth) = health()
    val (playerHealthPanel, getPlayerHealth, updatePlayerHealth) = health()
    val (maxHealthPanel, getMaxHealth) = maxHealth: limit =>
      updateDealerHealth.by(limit)
      updatePlayerHealth.by(limit)
    val (liveShellsPanel, getLiveShells) = shells()
    val (blankShellsPanel, getBlankShells) = shells()
    val (dealerItemsPanel, getDealerItems) = items()
    val (playerItemsPanel, getPlayerItems) = items()
    val (turnPanel, getTurn) = turn()

    def get(): StateForm = StateForm(
      for
        maxHealth <- getMaxHealth().limit
        live <- getLiveShells().shells
        blank <- getBlankShells().shells
        dealerHealth <- getDealerHealth().health if dealerHealth <= maxHealth
        playerHealth <- getPlayerHealth().health if playerHealth <= maxHealth
        turnOf <- getTurn().side
      yield GameState(
        maxHealth = maxHealth,
        shotgun = Shotgun(
          shells = ShellDistribution(
            live = live,
            blank = blank,
          ),
          effects = Shotgun.Effects.Default,
        ),
        player = Participant(
          health = playerHealth,
          items = getPlayerItems().items,
          hands = Hands.Free,
          revealed = Revealed(),
        ),
        dealer = Participant(
          health = dealerHealth,
          items = getDealerItems().items,
          hands = Hands.Free,
          revealed = Revealed(),
        ),
        turnOf = turnOf,
      ),
    )

    import GridLayout.*
    def separation() =
      Seq.fill(2)(Separator(Direction.HORIZONTAL).setLayoutData(createHorizontallyFilledLayoutData()))

    val panel = Panel(GridLayout(2)).withSeq:
      Seq(
        Seq(
          Label("Max Health").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          maxHealthPanel.withBorder(Borders.singleLine()),
        ),
        separation(),
        Seq(
          Label("Shotgun Live").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          liveShellsPanel.withBorder(Borders.singleLine()),
        ),
        Seq(
          Label("Shotgun Blank").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          blankShellsPanel.withBorder(Borders.singleLine()),
        ),
        separation(),
        Seq(
          Label("Dealer Health").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          dealerHealthPanel.withBorder(Borders.singleLine()),
        ),
        Seq(
          Label("Dealer Items").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          dealerItemsPanel,
        ),
        separation(),
        Seq(
          Label("Player Health").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          playerHealthPanel.withBorder(Borders.singleLine()),
        ),
        Seq(
          Label("Player Items").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          playerItemsPanel,
        ),
        separation(),
        Seq(
          Label("Turn").setLayoutData(createLayoutData(Alignment.BEGINNING, Alignment.CENTER)),
          turnPanel,
        ),
      ).flatten

    (panel, get)

  private case class ShellsForm(shells: Option[Nat])
  private def shells(): (ComboBox[Nat], () => ShellsForm) =
    val box = ComboBox[Nat](
      Nat[1],
      Nat[2],
      Nat[3],
      Nat[4],
    )
    def get(): ShellsForm = ShellsForm(Option(box.getSelectedItem))
    (box, get)

  private case class HealthForm(health: Option[Health])
  private trait UpdateHealth:
    def by(limit: HealthLimit): Unit

  private def health(): (ComboBox[Health], () => HealthForm, UpdateHealth) =
    val available: Seq[Health] = Seq(
      Health[1],
      Health[2],
      Health[3],
      Health[4],
      Health[5],
    )
    val box = ComboBox[Health](available*)
    box.setSelectedItem(Health[5])

    val update: UpdateHealth = (limit: HealthLimit) =>
      val current: Option[Health] = Option(box.getSelectedItem)
      box.clearItems()
      available.filter(_ <= limit).foreach(box.addItem)
      val updated = current.map(limit.cap).getOrElse(limit.max)
      box.setSelectedItem(updated)

    def get(): HealthForm = HealthForm(Option(box.getSelectedItem))
    (box, get, update)

  private case class MaxHealthForm(limit: Option[HealthLimit])
  private def maxHealth(update: UpdateHealth): (ComboBox[HealthLimit], () => MaxHealthForm) =
    val available: Seq[HealthLimit] = Seq(
      HealthLimit[2],
      HealthLimit[3],
      HealthLimit[4],
      HealthLimit[5],
    )
    val box = ComboBox[HealthLimit](available*)
    box.setSelectedItem(HealthLimit[5])
    def get(): MaxHealthForm = MaxHealthForm(Option(box.getSelectedItem))
    box.addListener: (index, _, _) =>
      update.by(available(index))
    (box, get)

  private case class TurnForm(side: Option[Side])
  private def turn(): (RadioBoxList[Side], () => TurnForm) =
    val select = RadioBoxList[Side]()
    select.addItem(Player)
    select.addItem(Dealer)
    select.setCheckedItem(Player)
    def get(): TurnForm = TurnForm(Option(select.getCheckedItem))
    (select, get)

  private case class ItemsForm(items: Items)
  private def items(): (Panel, () => ItemsForm) =
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

    def get(): ItemsForm =
      val selected =
        Seq(box1, box2, box3, box4, box5, box6, box7, box8)
          .map(box => Option(box.getSelectedItem))
          .collect:
            case Some(item: Item) => item
      ItemsForm(Items(selected*))

    Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
      Panel(GridLayout(2)).withAll(
        box1.withBorder(Borders.singleLine()),
        box2.withBorder(Borders.singleLine()),
        box3.withBorder(Borders.singleLine()),
        box4.withBorder(Borders.singleLine()),
      ),
      EmptySpace(),
      Panel(GridLayout(2)).withAll(
        box5.withBorder(Borders.singleLine()),
        box6.withBorder(Borders.singleLine()),
        box7.withBorder(Borders.singleLine()),
        box8.withBorder(Borders.singleLine()),
      ),
    ) -> get
