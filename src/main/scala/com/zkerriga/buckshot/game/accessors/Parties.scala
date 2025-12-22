package com.zkerriga.buckshot.game.accessors

import com.zkerriga.buckshot.game.state.partitipant.Participant

trait Parties[A]:
  def getPlayer(a: A): Participant
  def getDealer(a: A): Participant

  extension (a: A)
    def player: Participant = getPlayer(a)
    def dealer: Participant = getDealer(a)

object Parties:
  def of[A](playerIs: A => Participant, dealerIs: A => Participant): Parties[A] =
    new:
      def getPlayer(a: A): Participant = playerIs(a)
      def getDealer(a: A): Participant = dealerIs(a)
