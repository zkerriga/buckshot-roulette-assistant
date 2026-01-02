package com.zkerriga.types

object Opt:
  object syntax:
    given [A]: Conversion[A, Some[A]] = Some(_)
