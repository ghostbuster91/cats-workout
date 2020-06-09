package io.ghostbuster91.cats.workout.part4

import cats.{Id, Monad}

import cats.implicits._

object IdMonad {

  implicit def anyToId[T](t: T): Id[T] = t

  def main(args: Array[String]): Unit = {
    val a = 1
//    a.map(_ + 1)
  }
}
