package com.zkerriga.buckshot.game.state.partitipant

import com.zkerriga.types.Quantity

opaque type Health = Quantity

object Health:
  extension (health: Health)
    def damaged(by: Damage): Option[Health] =
      by match
        case Damage.Single => health.decrease
        case Damage.Double => health.decrease.flatMap(_.decrease)
