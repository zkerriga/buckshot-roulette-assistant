package com.zkerriga.buckshot.game

object all:
  export state.TableState
  export state.HealthLimit

  export state.shotgun.Shotgun
  export state.shotgun.Shell
  export state.shotgun.Shell.{Live, Blank}
  export state.shotgun.SeqNr
  export state.shotgun.SeqNr.{Shell1, Shell2, Shell3, Shell4, Shell5, Shell6, Shell7, Shell8}

  export state.items.{RegularItem, Item, Adrenaline, Slot}
  export state.items.Slot.{Slot1, Slot2, Slot3, Slot4, Slot5, Slot6, Slot7, Slot8}
  export state.items.RegularItem.{Handcuffs, MagnifyingGlass, Beer, Cigarettes, Saw, Inverter, BurnerPhone, Meds}

  export state.partitipant.{Damage, Hands, Health, Items, Participant, Side}
  export state.partitipant.Side.{Player, Dealer}
  export state.partitipant.Items.ItemOn
