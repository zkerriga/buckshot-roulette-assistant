package com.zkerriga.buckshot

import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.state.GameState
import com.zkerriga.buckshot.game.state.partitipant.{Health, Items, Participant}
import com.zkerriga.types.{Nat, Quantity}

object Data:
  val TestState = GameState(
    maxHealth = HealthLimit[5],
    player = Participant(
      health = Health[3],
      items = Items(
        Handcuffs,
        Handcuffs,
        MagnifyingGlass,
        Cigarettes,
        Adrenaline,
        Inverter,
        BurnerPhone,
        Meds,
      ),
      hands = Hands.Free,
      revealed = Revealed(Shell2 -> Live, Shell5 -> Blank),
    ),
    dealer = Participant(
      health = Health[2],
      items = Items(
        MagnifyingGlass,
        Beer,
        Beer,
        Cigarettes,
        Saw,
        BurnerPhone,
      ),
      hands = Hands.CuffedForOneShot,
      revealed = Revealed(Shell1 -> Blank),
    ),
    shotgun = Shotgun(
      shells = ShellDistribution(live = Nat[3], blank = Nat[4]),
      effects = Effects(
        damage = Damage.Double,
        inverted = true,
      ),
    ),
    turnOf = Player,
  )
