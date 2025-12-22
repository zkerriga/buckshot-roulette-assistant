package com.zkerriga.buckshot.game.state.partitipant

import com.zkerriga.buckshot.game.events.outcome.ErrorMsg.*
import com.zkerriga.buckshot.game.state.items.{Item, RegularItem}
import com.zkerriga.types.Quantity

opaque type Items = Map[Item, Quantity]

object Items:
  def apply(items: Item*): Items =
    items.foldLeft(Map.empty[Item, Quantity]): (owned, item) =>
      owned.updatedWith(item):
        case Some(existing) => Some(existing.increased)
        case None => Some(Quantity[1])

  extension (owned: Items)
    def contain(item: Item): Boolean = owned.contains(item)
    def removed(item: Item): V[Items] =
      (for
        quantity <- owned.get(item)
        newQuantity <- quantity.decreased
      yield owned.updated(item, newQuantity)).toRight(MissingItems)

    def getRegular: Set[RegularItem] =
      owned.keySet.collect:
        case item: RegularItem => item
