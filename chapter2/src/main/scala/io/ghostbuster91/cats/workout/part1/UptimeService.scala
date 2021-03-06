package io.ghostbuster91.cats.workout.part1

import cats.Applicative

import cats.syntax.traverse._
import cats.instances.list._
import cats.syntax.functor._

class UptimeService[F[_]:Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}
