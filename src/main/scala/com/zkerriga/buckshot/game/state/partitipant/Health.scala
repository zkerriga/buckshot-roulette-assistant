package com.zkerriga.buckshot.game.state.partitipant

import com.zkerriga.buckshot.game.state.HealthLimit
import com.zkerriga.types.Quantity

opaque type Health = Quantity

object Health:
  inline def apply(value: Quantity): Health = value

  extension (health: Health)
    def healed(by: Heal, limit: HealthLimit): Health =
      limit.cap:
        by match
          case Heal.Single => health.increased
          case Heal.Double => health.increased.increased

    def damaged(by: Damage): Option[Health] =
      by match
        case Damage.Single => health.decreased
        case Damage.Double => health.decreased.flatMap(_.decreased)

  given Ordering[Health] = summon[Ordering[Quantity]]
  given Conversion[Health, Int] = summon[Conversion[Quantity, Int]].apply

  inline def apply[N <: Int: ValueOf]: Health = Quantity[N]
