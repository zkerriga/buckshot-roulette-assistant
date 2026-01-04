package com.zkerriga.buckshot.engine.events

import com.zkerriga.buckshot.game.events.ItemUse
import com.zkerriga.buckshot.game.state.shotgun.{SeqNr, Shell}

enum FullItemUse:
  case Handcuffs
  case MagnifyingGlass(revealed: Shell)
  case Beer(out: Shell)
  case Cigarettes
  case Saw
  case Inverter
  case BurnerPhone(revealed: Option[(revealed: Shell, at: SeqNr)])
  case Meds(good: Boolean)

object FullItemUse:
  extension (use: FullItemUse)
    def toPublic: ItemUse = use match
      case Handcuffs => ItemUse.Handcuffs
      case MagnifyingGlass(revealed) => ItemUse.MagnifyingGlass
      case Beer(out) => ItemUse.Beer(out)
      case Cigarettes => ItemUse.Cigarettes
      case Saw => ItemUse.Saw
      case Inverter => ItemUse.Inverter
      case BurnerPhone(revealed) => ItemUse.BurnerPhone
      case Meds(good) => ItemUse.Meds(good)
