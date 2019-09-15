package io.ghostbuster91.cats.workout

import cats.data.EitherT
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object MonadTransformer {

  type Response[A] = Future[Either[String, A]]

  private val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def main(array: Array[String]): Unit = {
    println(getPowerLevel("Jazz"))
    println(getPowerLevel("Jazz2"))
    println(tacticalReport("Jazz","Jazz"))
    println(tacticalReport("Hot Rod","Jazz"))
  }

  def getPowerLevel(autobot: String): Response[Int] = {
    EitherT.fromOption[Future](powerLevels.get(autobot), s"robot $autobot not found").value
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    (for {
      a1 <- EitherT(getPowerLevel(ally1))
      a2 <- EitherT(getPowerLevel(ally2))
    } yield a1 + a2 > 15).value
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    Await.result(canSpecialMove(ally1,ally2),Duration.Inf) match {
      case Left(l)=>"no no"
      case Right(false)=>"no"
      case Right(true)=>"yes"
    }
  }
}
