package io.ghostbuster91.cats.workout

import cats.arrow.FunctionK
import cats.{Applicative, Foldable, Monad, MonoidK}
import cats.kernel.Monoid
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.instances.list._
import cats.syntax.semigroup._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import cats.instances.future._

import scala.concurrent.duration.Duration

object FoldExample {
  def main(args: Array[String]): Unit = {
//    println("kasper")
//    println(List(1,2,3).foldLeft(List.empty[Int])((acc,item)=>item :: acc))
//    println(List(1,2,3).foldRight(List.empty[Int])((item,acc)=>item :: acc))
//    println(map[Int, String](List(1, 2, 3), _.toString))
//    println(filter[Int](List(1, 2, 3), _ == 2))
//    println(flatMap[Int, Int](List(1, 2, 3), x=>List(x-1,x,x+1)))

    import cats.instances.vector._ // for Applicative
    println(listSequence(List(Vector(1, 2), Vector(3, 4))))

    import cats.instances.option._ // for Applicative
    def process(inputs: List[Int]) =
      listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

    println(process(List(2, 4, 6)))
    println(process(List(1, 2, 3)))

    val listToOption = λ[FunctionK[List, Option]](_.headOption)

    implicit def appendable: Appendable[List] = new Appendable[List] {
      override def append[E](s: List[E], e: E): List[E] = s :+ e
    }

    println(genTraverse(List(2, 4, 6))(n => if (n % 2 == 0) Some(n) else None))
    println(Await.result(genTraverse(List(2, 4, 6))(n => Future(n)), Duration.Inf))
  }

  def map[T, R](l: List[T], f: T => R) = {
    l.foldRight(List.empty[R])((item, acc) => f(item) :: acc)
  }

  def filter[T](l: List[T], p: T => Boolean) = {
    l.foldRight(List.empty[T])((item, acc) => if (p(item)) item :: acc else acc)
  }

  def flatMap[T, R](l: List[T], f: T => List[R]) = {
    l.foldRight(List.empty[R])((item, acc) => f(item) ++ acc)
  }

  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def genTraverse[F[_]: Applicative, G[_]: MonoidK: Foldable: Appendable, A, B](list: G[A])(func: A => F[B]): F[G[B]] =
    Foldable[G].foldLeft(list, MonoidK[G].empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN[G[B]]((a, b) => implicitly[Appendable[G]].append(a, b))
    }

  def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  trait Appendable[G[_]] {
    def append[E](s: G[E], e: E): G[E]
  }
}
