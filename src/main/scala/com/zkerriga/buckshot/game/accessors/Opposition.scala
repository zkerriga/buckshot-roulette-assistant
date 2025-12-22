package com.zkerriga.buckshot.game.accessors

import com.zkerriga.buckshot.game.state.partitipant.Participant

trait Opposition[A]:
  def getActor(a: A): Participant
  def getOpponent(a: A): Participant

  extension (a: A)
    def actor: Participant = getActor(a)
    def opponent: Participant = getOpponent(a)

object Opposition:
  def of[A](actorIs: A => Participant, opponentIs: A => Participant): Opposition[A] =
    new:
      def getActor(a: A): Participant = actorIs(a)
      def getOpponent(a: A): Participant = opponentIs(a)
