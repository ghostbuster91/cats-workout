package io.ghostbuster91.cats.workout

import cats.Monad

object SemigroupalExample {
  import cats.syntax.flatMap._
  import cats.syntax.functor._ // for map
  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
    for {
      a <- x
      b <- y
    } yield (a, b)
  }

  def main(args: Array[String]) = {
    import cats.instances.list._
    println(product(List(1, 2), List(3, 4)))
  }
}
