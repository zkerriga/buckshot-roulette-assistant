package com.zkerriga.buckshot.game.state.items

enum RegularItem:
  case Handcuffs, MagnifyingGlass, Beer, Cigarettes, Saw, Inverter, BurnerPhone, Meds

object RegularItem:
  val All: Set[RegularItem] = values.toSet
