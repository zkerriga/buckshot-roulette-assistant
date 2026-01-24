package com.zkerriga.types.boundaries

import scala.util.boundary, boundary.break, boundary.Label

object optional:
  inline def apply[A](inline body: Label[None.type] ?=> A): Option[A] =
    boundary(Some(body))

  extension [A](value: Option[A])
    inline def ?(using label: Label[None.type]): A = value match
      case Some(x) => x
      case None => break(None)
