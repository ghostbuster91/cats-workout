package io.ghostbuster91.cats.workout

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Writer {
  type Logged[A] = Writer[Vector[String], A]


  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial1(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial1(n - 1))
    println(s"fact $n $ans")
    ans
  }

  def factorial(n: Int): Logged[Int] = {
    for {
      _ <- Vector(s"enter fact $n").tell
      w <- if (n == 0) {
        1.pure[Logged]
      } else {
        factorial(n - 1).map(_ * n)
      }
      _ <- Vector(s"exit fact $n $w").tell
    } yield w
  }

  def main(args: Array[String]): Unit = {
    Await.result(Future.sequence(Vector(
      Future(factorial(5)).map { z => println(z.run); z },
      Future(factorial(5)).map { z => println(z.run); z }
    )), 5.seconds)

    Await.result(Future.sequence(Vector(
      Future(factorial1(5)),
      Future(factorial1(5))
    )), 5.seconds)
  }
}
