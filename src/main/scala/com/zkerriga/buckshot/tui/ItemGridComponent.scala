package com.zkerriga.buckshot.tui

import com.googlecode.lanterna.gui2.*
import com.googlecode.lanterna.gui2.GridLayout.{Alignment, createLayoutData}
import com.zkerriga.buckshot.game.all.*

object ItemGridComponent:
  def render[A](items: Items)(renderer: (Option[Item], Slot) => Component): Panel = {
    def gridOf(slots: Seq[Slot]): Panel =
      import GridLayout.*
      Panel(GridLayout(2)).withSeq(
        slots.map { slot =>
          renderer(items.on(slot), slot)
            .setLayoutData(createLayoutData(Alignment.CENTER, Alignment.CENTER))
            .withBorder(Borders.singleLine())
        },
      )

    Panel(LinearLayout(Direction.HORIZONTAL)).withAll(
      gridOf(Seq(Slot1, Slot2, Slot3, Slot4)),
      EmptySpace(),
      gridOf(Seq(Slot5, Slot6, Slot7, Slot8)),
    )
  }

  extension (item: Item)
    def labelName: String =
      item match {
        case Adrenaline => "Adrenaline"
        case Handcuffs => "Handcuffs"
        case MagnifyingGlass => "Magnifying Glass"
        case Beer => "Beer"
        case Cigarettes => "Cigarettes"
        case Saw => "Saw"
        case Inverter => "Inverter"
        case BurnerPhone => "BurnerPhone"
        case Meds => "Meds"
      }
