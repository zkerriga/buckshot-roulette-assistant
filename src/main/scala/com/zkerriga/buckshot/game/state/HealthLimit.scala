package com.zkerriga.buckshot.game.state

import com.zkerriga.types.Quantity

opaque type HealthLimit = Quantity

object HealthLimit:
  inline def apply[N <: Int: ValueOf]: HealthLimit = Quantity[N]
