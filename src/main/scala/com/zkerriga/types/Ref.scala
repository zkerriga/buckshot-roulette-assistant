package com.zkerriga.types

// todo: replace with real thread-safe reference later
class Ref[A] private (var value: A):
  def get: A = value
  def modify[B](f: A => (A, B)): B =
    val (updated, result) = f(value)
    value = updated
    result

object Ref:
  def of[A](initial: A): Ref[A] = Ref(initial)
