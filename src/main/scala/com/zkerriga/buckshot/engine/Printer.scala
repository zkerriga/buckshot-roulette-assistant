package com.zkerriga.buckshot.engine

import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state.TableState
import com.zkerriga.buckshot.game.state.partitipant.{Items, Participant}

object Printer:
  def print(table: TableState): String =
    s"TableState(maxHealth=HealthLimit[${table.maxHealth}],turn=${table.turn},dealer=${print(table.dealer)},shotgun=${print(table.shotgun)},player=${print(table.player)})"

  def print(p: Participant): String =
    s"Participant(health=Health[${p.health}],items =${print(p.items)},hands=Hands.${p.hands})"

  def print(s: Shotgun): String =
    s"Shotgun(shells=ShellDistribution(live=Nat[${s.live}], Nat[${s.blank}]),effects=Effects(damage = Damage.${s.damage},inverted=${s.inverted}))"

  def print(items: Items): String = {
    val positioned = items.positioned.map { itemOn =>
      s"ItemOn(item=${itemOn.item},on=${itemOn.on})"
    }
    s"Items(adrenaline=${items.adrenaline},positioned=$positioned)"
  }
