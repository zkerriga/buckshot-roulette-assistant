package com.zkerriga.types.steps

import steps.result.Result

import scala.util.boundary

object ResultExtension:
  class Raise[-E](private val label: boundary.Label[Result[Nothing, E]]):
    inline def raise(error: E): Nothing =
      boundary.break(Result.Err(error))(using label)

  object Raise:
    extension [E](error: E)(using raise: Raise[E]) inline def raise: Nothing = raise.raise(error)
    extension [A, E: Raise](result: Result[A, E])
      inline def unwrap: A =
        result match
          case Result.Ok(value) => value
          case Result.Err(error) => error.raise

  object AnyValueOps:
    extension [A](value: A)
      inline def asOk: Result.Ok[A] = Result.Ok(value)
      inline def asError: Result.Err[A] = Result.Err(value)

  object BooleanOps:
    extension (condition: Boolean)
      infix def trueOrRaise[E: Raise](error: E): Unit =
        if condition then () else error.raise

  object OptionOps:
    extension [A](opt: Option[A])
      inline def getOrRaise[E: Raise](error: E): A =
        opt match
          case Some(value) => value
          case None => error.raise

  object ResultOps:
    extension (o: Result.type)
      inline def scope[A, E](inline body: Raise[E] ?=> A): Result[A, E] =
        boundary: label ?=>
          Result.Ok(body(using Raise[E](label)))

  export Raise.*
  export ResultOps.*
  export AnyValueOps.*
  export BooleanOps.*
  export OptionOps.*
