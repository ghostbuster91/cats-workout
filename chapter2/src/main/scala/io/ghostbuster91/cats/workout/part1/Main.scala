package io.ghostbuster91.cats.workout.part1

import cats.Functor

object Main {
  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  def main(args: Array[String]) = {
    testTotalUptime()

    import cats.implicits._

    println(Functor[List].map(List(1, 2, 3, 4))(x => x + 1))

    val f1 = Functor[List]
    val f2 = Functor[Option]
    (f1 compose f2).map(List(Option(1)))(x => x)

  }
}
