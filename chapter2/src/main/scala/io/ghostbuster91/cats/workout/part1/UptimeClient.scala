package io.ghostbuster91.cats.workout.part1

import cats.Id

import scala.concurrent.Future

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class RealUptimeClient extends UptimeClient[Future] {
  override def getUptime(hostname: String): Future[Int] = {
    ???
  }
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int] =
    hosts.getOrElse(hostname, 0)
}
