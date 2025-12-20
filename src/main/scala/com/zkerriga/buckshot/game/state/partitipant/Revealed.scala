package com.zkerriga.buckshot.game.state.partitipant

import com.zkerriga.buckshot.game.state.shotgun.SeqNr.*
import com.zkerriga.buckshot.game.state.shotgun.{SeqNr, Shell}

opaque type Revealed = Map[SeqNr, Shell]

object Revealed:
  def apply(revealed: (SeqNr, Shell)*): Revealed = revealed.toMap

  extension (revealed: Revealed)
    def get(seqNr: SeqNr): Option[Shell] = revealed.get(seqNr)
    def revealed(shell: Shell, at: SeqNr): Revealed = revealed.updated(at, shell)

    def postShot: Revealed =
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
