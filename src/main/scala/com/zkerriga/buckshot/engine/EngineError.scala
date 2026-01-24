package com.zkerriga.buckshot.engine

import com.zkerriga.buckshot.game.events.outcome.ErrorMsg

enum EngineError:
  case BadConditioning

object EngineError:
  type V[+A] = Either[ErrorMsg | EngineError, A]

  extension [A](value: A) inline def ok: Right[Nothing, A] = Right(value)
