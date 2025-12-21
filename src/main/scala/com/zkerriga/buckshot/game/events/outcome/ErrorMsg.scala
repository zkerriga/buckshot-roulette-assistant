package com.zkerriga.buckshot.game.events.outcome

enum ErrorMsg:
  case WrongTurn
  case ShotgunStateMismatch
  case MissingItems
  case SawAlreadyUsed

object ErrorMsg:
  type V[+A] = Either[ErrorMsg, A]

  extension [A](value: A) def ok: V[A] = Right(value)
  extension (error: ErrorMsg) def lift: V[Nothing] = Left(error)

  extension (condition: Boolean)
    infix def trueOr(error: ErrorMsg): V[Unit] =
      Either.cond(condition, (), error)
