package io.ghostbuster91.cats.workout

import cats.data.OptionT

import scala.concurrent.Future
import cats.syntax.either._

import scala.concurrent.ExecutionContext.Implicits.global
import cats.syntax.option._
import cats.syntax.applicative._
import cats.instances.future._
object MonadTransformer {

  type Response[A] = Future[Either[String, A]]

  def main(array:Array[String]):Unit = {

    println (getPowerLevel("Jazz"))
  }

  def getPowerLevel(autobot: String): Response[Int] = {
    val powerLevels = Map(
      "Jazz"
        -> 6,
      "Bumblebee" -> 8,
      "Hot Rod"
        -> 10
    )
    powerLevels.get(autobot).fold("unreachable".asLeft[Int])(_.asRight[String]).pure[Future]
    for {
      _ <- OptionT. powerLevels.get(autobot)
    }yield
  }
}
