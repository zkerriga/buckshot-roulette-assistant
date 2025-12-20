package com.zkerriga.buckshot.board.partitipant

import com.zkerriga.buckshot.board.shotgun.{SeqNr, Shell}

opaque type Revealed = Map[SeqNr, Shell]

object Revealed:
  def apply(revealed: (SeqNr, Shell)*): Revealed = revealed.toMap

  extension (revealed: Revealed)
    def get(seqNr: SeqNr): Option[Shell] = revealed.get(seqNr)
    def revealed(shell: Shell, at: SeqNr): Revealed = revealed.updated(at, shell)
