package io.ghostbuster91.cats.workout.part2

import cats.kernel.Monoid
import cats.instances.int._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.traverse._
import cats.instances.vector._
import cats.instances.future._
import cats.syntax.foldable._
import scala.concurrent.duration.Duration

object AAA {
  def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B) = {
    val monoidB = Monoid[B]
    seq.map(f).foldLeft(monoidB.empty)(monoidB.combine)
  }

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val cores = Runtime.getRuntime.availableProcessors()
    val groupSize = (1.0 * values.size / cores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector
      .traverse(g => Future.apply(foldMap(g)(func)))
      .map(_.combineAll)
  }

  def main(args: Array[String]): Unit = {
    import cats.instances.string._
    println(foldMap(Vector(1, 2, 3))(identity))
    println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
    println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))

    println(Await.result(parallelFoldMap(Vector(1, 2, 3))(identity), Duration.Inf))
    println(Await.result(parallelFoldMap(Vector(1, 2, 3))(_.toString + "! "), Duration.Inf))
    println(Await.result(parallelFoldMap("Hello world!".toVector)(_.toString.toUpperCase), Duration.Inf))
  }
}
