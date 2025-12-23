package com.zkerriga.buckshot.game

object all:
  export state.GameState
  export state.HealthLimit

  export state.shotgun.Shotgun
  export state.shotgun.Shell.{Live, Blank}
  export state.shotgun.SeqNr.{Shell1, Shell2, Shell3, Shell4, Shell5, Shell6, Shell7, Shell8}
  export state.shotgun.Shotgun.{Effects, ShellDistribution}

  export state.items.RegularItem.{Handcuffs, MagnifyingGlass, Beer, Cigarettes, Saw, Inverter, BurnerPhone, Meds}
  export state.items.Adrenaline

  export state.partitipant.{Damage, Hands, Health, Items, Participant, Revealed}
  export state.partitipant.Side.{Player, Dealer}
