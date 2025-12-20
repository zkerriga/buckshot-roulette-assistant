package com.zkerriga.buckshot.game.state

import com.zkerriga.types.Quantity

opaque type HealthLimit = Quantity

object HealthLimit:
  inline def apply(value: Quantity): HealthLimit = value
