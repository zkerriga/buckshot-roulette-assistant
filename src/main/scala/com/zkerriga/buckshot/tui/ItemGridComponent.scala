package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.zkerriga.buckshot.game.all.*

object ItemGridComponent:
  def render[A](items: Items)(renderer: (Option[Item], Slot) => Component): Panel = {
    def gridOf(topLeft: Slot, topRight: Slot, bottomLeft: Slot, bottonRight: Slot) =
      import GridLayout.*
      def renderSlot(slot: Slot) = renderer(items.on(slot), slot)
      Panel(GridLayout(3))
        .withAll(
          renderSlot(topLeft),
          Separator(Direction.VERTICAL),
          renderSlot(topRight),
          Separator(Direction.HORIZONTAL).setLayoutData(createHorizontallyFilledLayoutData()),
          Separator(Direction.VERTICAL),
          Separator(Direction.HORIZONTAL).setLayoutData(createHorizontallyFilledLayoutData()),
          renderSlot(bottomLeft),
          Separator(Direction.VERTICAL),
          renderSlot(bottonRight),
        )
        .withBorder(Borders.singleLine())

    Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
      gridOf(Slot1, Slot2, Slot3, Slot4),
      EmptySpace(),
      gridOf(Slot5, Slot6, Slot7, Slot8),
    )
  }

  extension (item: Item)
    def labelName: String =
      item match {
        case Adrenaline => "Adrenaline"
        case Handcuffs => "Handcuffs "
        case MagnifyingGlass => "Mag Glass "
        case Beer => "Beer      "
        case Cigarettes => "Cigarettes"
        case Saw => "Saw       "
        case Inverter => "Inverter  "
        case BurnerPhone => "Burn Phone"
        case Meds => "Meds      "
      }

  val NoItem: String = "        "
