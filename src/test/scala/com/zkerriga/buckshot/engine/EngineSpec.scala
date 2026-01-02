package com.zkerriga.buckshot.engine

import com.zkerriga.buckshot.engine.Engine.EventReply
import com.zkerriga.buckshot.game.all.*
import com.zkerriga.buckshot.game.events.Used.ItemUse
import com.zkerriga.buckshot.game.events.{Shot, Used}
import com.zkerriga.types.Nat
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EngineSpec extends AnyWordSpec, Matchers:
  "Engine" should:
    "process all events from a valid game" in:
      val initial = TableState(
        maxHealth = HealthLimit[3],
        turn = Player,
        dealer = Participant(
          health = Health[3],
          items = Items(
            MagnifyingGlass,
            Meds,
            Cigarettes,
          ),
          hands = Hands.Free,
        ),
        shotgun = Shotgun.fresh(live = Nat[2], blank = Nat[3]),
        player = Participant(
          health = Health[3],
          items = Items(
            Meds,
            Cigarettes,
            Inverter,
          ),
          hands = Hands.Free,
        ),
      )
      val engine = Engine.start(initial)

      engine.process(Shot(actor = Player, target = Player, shell = Blank)).isRight mustBe true
      engine.process(Shot(actor = Player, target = Dealer, shell = Live)).isRight mustBe true
      engine.process(Used(actor = Dealer, item = ItemUse.Cigarettes, stolen = false)).isRight mustBe true
      engine.process(Used(actor = Dealer, item = ItemUse.MagnifyingGlass, stolen = false)).isRight mustBe true
      engine.process(Shot(actor = Dealer, target = Dealer, shell = Blank)).isRight mustBe true
      engine.process(Shot(actor = Dealer, target = Dealer, shell = Live)).isRight mustBe true
      val result = engine.process(Shot(actor = Player, target = Dealer, shell = Blank))
      result match {
        case Left(error) => fail(s"unexpected: $error")
        case Right(reply) =>
          reply match {
            case EventReply.NewState(state, dealer) => fail(s"unexpected new state $state")
            case EventReply.GameOver(winner) => fail(s"unexpected winner $winner")
            case EventReply.ShotgunReset(reset) => succeed
          }
      }
