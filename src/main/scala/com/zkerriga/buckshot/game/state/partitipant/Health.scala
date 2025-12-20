package com.zkerriga.buckshot.game.state.partitipant

import com.zkerriga.types.Quantity

opaque type Health = Quantity

object Health:
  inline def apply(value: Quantity): Health = value

  extension (health: Health)
    def damaged(by: Damage): Option[Health] =
      by match
        case Damage.Single => health.decrease
        case Damage.Double => health.decrease.flatMap(_.decrease)

  inline def apply[N <: Int: ValueOf]: Health = Quantity[N]
