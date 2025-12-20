package com.zkerriga.buckshot.game.events.outcome

enum ErrorMsg:
  case WrongTurn
  case ShotgunStateMismatch
  // todo: more

object ErrorMsg:
  type V[+A] = Either[ErrorMsg, A]

  extension (condition: Boolean)
    infix def trueOr(error: ErrorMsg): V[Unit] =
      Either.cond(condition, (), error)
