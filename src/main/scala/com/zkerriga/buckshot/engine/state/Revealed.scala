package com.zkerriga.buckshot.engine.state

import cats.Eq
import com.zkerriga.buckshot.game.state.shotgun.SeqNr.*
import com.zkerriga.buckshot.game.state.shotgun.{SeqNr, Shell}
import com.zkerriga.types.Nat
import com.zkerriga.types.Nat.countNat

opaque type Revealed = Map[SeqNr, Shell]

object Revealed:
  val Nothing: Revealed = Map.empty

  def apply(revealed: (SeqNr, Shell)*): Revealed = revealed.toMap

  extension (revealed: Revealed)
    def get(seqNr: SeqNr): Option[Shell] = revealed.get(seqNr)
    def count(shell: Shell): Nat = revealed.values.countNat(_ == shell)
    def except(at: SeqNr): Revealed = revealed - at
    def combineWith(other: Revealed): Revealed = revealed ++ other

    def revealed(shell: Shell, at: SeqNr): Revealed = revealed.updated(at, shell)

    def afterShellOut: Revealed =
      revealed.flatMap: (seqNr, shell) =>
        val moved = seqNr match
          case Shell1 => None
          case Shell2 => Some(Shell1)
          case Shell3 => Some(Shell2)
          case Shell4 => Some(Shell3)
          case Shell5 => Some(Shell4)
          case Shell6 => Some(Shell5)
          case Shell7 => Some(Shell6)
          case Shell8 => Some(Shell7)
        moved.map(_ -> shell)

  given Eq[Revealed] = Eq.fromUniversalEquals
