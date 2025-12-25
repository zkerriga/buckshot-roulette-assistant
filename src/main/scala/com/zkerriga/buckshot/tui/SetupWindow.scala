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
    val content = Panel(LinearLayout(Direction.VERTICAL))
    val (compositionPanel, getState) = composition()
    content.addComponent(compositionPanel)
    content.addComponent(
      Button(
        "Start",
        () =>
          getState().state match
            case Some(state) =>
              setState(state)
              window.close()
            case None => (), // todo: log error
      ),
      LinearLayout.createLayoutData(LinearLayout.Alignment.End),
    )
    content.addComponent(quit(window), LinearLayout.createLayoutData(LinearLayout.Alignment.End))
    window.setComponent(content)
    window

  def quit(window: Window): Button =
    Button(
      "Quit",
      () => window.close(),
    )

  case class StateForm(state: Option[GameState])
  def composition(): (Panel, () => StateForm) =
    val panel = Panel(GridLayout(2))

    def addSeparation() =
      panel.addComponent(Separator(Direction.HORIZONTAL).setLayoutData(GridLayout.createHorizontallyFilledLayoutData()))
      panel.addComponent(Separator(Direction.HORIZONTAL).setLayoutData(GridLayout.createHorizontallyFilledLayoutData()))

    panel.addComponent(
      Label("Max Health"),
      GridLayout.createLayoutData(
        GridLayout.Alignment.BEGINNING,
        GridLayout.Alignment.CENTER,
      ),
    )
    val (dealerHealthPanel, getDealerHealth, updateDealerHealth) = health()
    val (playerHealthPanel, getPlayerHealth, updatePlayerHealth) = health()
    val (maxHealthPanel, getMaxHealth) = maxHealth: limit =>
      updateDealerHealth.by(limit)
      updatePlayerHealth.by(limit)
    panel.addComponent(maxHealthPanel.withBorder(Borders.singleLine()))

    addSeparation()

    panel.addComponent(
      Label("Shotgun Live"),
      GridLayout.createLayoutData(
        GridLayout.Alignment.BEGINNING,
        GridLayout.Alignment.CENTER,
      ),
    )
    val (liveShellsPanel, getLiveShells) = shells()
    panel.addComponent(liveShellsPanel.withBorder(Borders.singleLine()))
    panel.addComponent(
      Label("Shotgun Blank"),
      GridLayout.createLayoutData(
        GridLayout.Alignment.BEGINNING,
        GridLayout.Alignment.CENTER,
      ),
    )
    val (blankShellsPanel, getBlankShells) = shells()
    panel.addComponent(blankShellsPanel.withBorder(Borders.singleLine()))

    addSeparation()

    panel.addComponent(
      Label("Dealer Health"),
      GridLayout.createLayoutData(
        GridLayout.Alignment.BEGINNING,
        GridLayout.Alignment.CENTER,
      ),
    )
    panel.addComponent(dealerHealthPanel.withBorder(Borders.singleLine()))
    panel.addComponent(
      Label("Dealer Items"),
      GridLayout.createLayoutData(
        GridLayout.Alignment.BEGINNING,
        GridLayout.Alignment.CENTER,
      ),
    )
    val (dealerItemsPanel, getDealerItems) = items()
    panel.addComponent(dealerItemsPanel)

    addSeparation()

    panel.addComponent(
      Label("Player Health"),
      GridLayout.createLayoutData(
        GridLayout.Alignment.BEGINNING,
        GridLayout.Alignment.CENTER,
      ),
    )
    panel.addComponent(playerHealthPanel.withBorder(Borders.singleLine()))
    panel.addComponent(
      Label("Player Items"),
      GridLayout.createLayoutData(
        GridLayout.Alignment.BEGINNING,
        GridLayout.Alignment.CENTER,
      ),
    )
    val (playerItemsPanel, getPlayerItems) = items()
    panel.addComponent(playerItemsPanel)

    addSeparation()

    panel.addComponent(
      Label("Turn"),
      GridLayout.createLayoutData(
        GridLayout.Alignment.BEGINNING,
        GridLayout.Alignment.CENTER,
      ),
    )
    val (turnPanel, getTurn) = turn()
    panel.addComponent(turnPanel)

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

    (panel, get)

  case class ShellsForm(shells: Option[Nat])
  def shells(): (ComboBox[Nat], () => ShellsForm) =
    val box = ComboBox[Nat](
      Nat[1],
      Nat[2],
      Nat[3],
      Nat[4],
    )
    def get(): ShellsForm = ShellsForm(Option(box.getSelectedItem))
    (box, get)

  case class HealthForm(health: Option[Health])
  trait UpdateHealth:
    def by(limit: HealthLimit): Unit

  def health(): (ComboBox[Health], () => HealthForm, UpdateHealth) =
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

  case class MaxHealthForm(limit: Option[HealthLimit])
  def maxHealth(update: UpdateHealth): (ComboBox[HealthLimit], () => MaxHealthForm) =
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

  case class TurnForm(side: Option[Side])
  def turn(): (RadioBoxList[Side], () => TurnForm) =
    val select = RadioBoxList[Side]()
    select.addItem(Player)
    select.addItem(Dealer)
    select.setCheckedItem(Player)
    def get(): TurnForm = TurnForm(Option(select.getCheckedItem))
    (select, get)

  case class ItemsForm(items: Items)
  def items(): (Panel, () => ItemsForm) =
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

    val panel = Panel(LinearLayout(Direction.HORIZONTAL))

    val left = Panel(GridLayout(2))
    left.addComponent(box1.withBorder(Borders.singleLine()))
    left.addComponent(box2.withBorder(Borders.singleLine()))
    left.addComponent(box3.withBorder(Borders.singleLine()))
    left.addComponent(box4.withBorder(Borders.singleLine()))

    val right = Panel(GridLayout(2))
    right.addComponent(box5.withBorder(Borders.singleLine()))
    right.addComponent(box6.withBorder(Borders.singleLine()))
    right.addComponent(box7.withBorder(Borders.singleLine()))
    right.addComponent(box8.withBorder(Borders.singleLine()))

    panel.addComponent(left)
    panel.addComponent(EmptySpace())
    panel.addComponent(right)

    def get(): ItemsForm =
      val selected =
        Seq(box1, box2, box3, box4, box5, box6, box7, box8)
          .map(box => Option(box.getSelectedItem))
          .collect:
            case Some(item: Item) => item
      ItemsForm(Items(selected*))

    (panel, get)
