package com.zkerriga.buckshot.game.events

import com.zkerriga.buckshot.game.state.items.RegularItem
import com.zkerriga.buckshot.game.state.shotgun.Shell

enum ItemUse:
  case Handcuffs
  case MagnifyingGlass
  case Beer(out: Shell)
  case Cigarettes
  case Saw
  case Inverter
  case BurnerPhone
  case Meds(good: Boolean)

object ItemUse:
  extension (use: ItemUse)
    def toItem: RegularItem = use match
      case Handcuffs => RegularItem.Handcuffs
      case MagnifyingGlass => RegularItem.MagnifyingGlass
      case _: Beer => RegularItem.Beer
      case Cigarettes => RegularItem.Cigarettes
      case Saw => RegularItem.Saw
      case Inverter => RegularItem.Inverter
      case BurnerPhone => RegularItem.BurnerPhone
      case _: Meds => RegularItem.Meds
