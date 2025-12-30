package com.zkerriga.buckshot.game.state

import com.zkerriga.buckshot.game.state.partitipant.Health

import scala.math.Ordering.Implicits.infixOrderingOps

opaque type HealthLimit = Health

object HealthLimit:
  inline def apply[N <: Int: ValueOf]: HealthLimit = Health[N]

  extension (limit: HealthLimit)
    def maxAllowed: Health = limit
    def allows(health: Health): Boolean = health <= limit
    def cap(health: Health): Health = limit min health
    def asInt: Int = Health.asInt(limit)
