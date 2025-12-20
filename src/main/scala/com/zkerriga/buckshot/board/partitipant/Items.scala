package com.zkerriga.buckshot.board.partitipant

import com.zkerriga.buckshot.board.items.{Item, RegularItem}
import com.zkerriga.types.Quantity

opaque type Items = Map[Item, Quantity]

object Items:
  def apply(items: Item*): Items =
    items.foldLeft(Map.empty[Item, Quantity]): (owned, item) =>
      owned.updatedWith(item):
        case Some(existing) => Some(existing.increase)
        case None => Some(Quantity.One)

  extension (owned: Items)
    def contain(item: Item): Boolean = owned.contains(item)
    def removed(item: Item): Items = owned.updatedWith(item)(_.flatMap(_.decrease))

    def getRegular: Set[RegularItem] =
      owned.keySet.collect:
        case item: RegularItem => item
