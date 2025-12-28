package com.zkerriga.buckshot.game.accessors

import com.zkerriga.buckshot.game.state.partitipant.Participant

trait Parties[A]:
  def getDealer(a: A): Participant
  def getPlayer(a: A): Participant

  extension (a: A)
    def dealer: Participant = getDealer(a)
    def player: Participant = getPlayer(a)

object Parties:
  def of[A](dealerIs: A => Participant, playerIs: A => Participant): Parties[A] =
    new:
      def getDealer(a: A): Participant = dealerIs(a)
      def getPlayer(a: A): Participant = playerIs(a)
