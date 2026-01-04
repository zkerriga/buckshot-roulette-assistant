package com.zkerriga.buckshot.game.state.partitipant

import com.zkerriga.buckshot.game.state.items.*
import com.zkerriga.buckshot.game.state.partitipant.Items.ItemOn

case class Items(
  adrenaline: Set[Slot],
  positioned: Seq[ItemOn],
)

object Items:
  case class ItemOn(item: RegularItem, on: Slot)

  val Empty: Items = Items(Set.empty, Seq.empty)
  def from(items: Seq[(item: Item, on: Slot)]): Items =
    val (adrenaline, positioned) = items.partitionMap:
      case (Adrenaline, slot) => Left(slot)
      case (item: RegularItem, slot) => Right(ItemOn(item, slot))
    Items(adrenaline.toSet, positioned)

  extension (owned: Items)
    def isEmpty: Boolean =
      owned.adrenaline.isEmpty && owned.positioned.isEmpty

    def contain(item: RegularItem): Boolean = owned.positioned.exists(_.item == item)
    def containAdrenaline: Boolean = owned.adrenaline.nonEmpty

    def without(item: ItemOn): Items =
      owned.copy(positioned = owned.positioned.filterNot(_ == item))

    def withoutAdrenaline(on: Slot): Items =
      owned.copy(adrenaline = owned.adrenaline - on)

    def on(slot: Slot): Option[Item] =
      owned.positioned
        .find(_.on == slot)
        .map(_.item)
        .orElse(Option.when(owned.adrenaline.contains(slot))(Adrenaline))
