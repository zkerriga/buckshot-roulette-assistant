package com.zkerriga.types.boundaries

import scala.util.boundary, boundary.break, boundary.Label

object eithers:
  inline def apply[A, B](inline body: Label[Left[A, Nothing]] ?=> B): Either[A, B] =
    boundary(Right(body))

  extension [A, B](value: Either[A, B])
    def ?(using label: Label[Left[A, Nothing]]): B = value match
      case Left(error) => break(Left(error))
      case Right(value) => value
