package io.ghostbuster91.cats.workout.part3

import cats.{ApplicativeError, SemigroupK}
import cats.data.{Kleisli, NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.{Monoid, Semigroup}
import cats.instances.either._
import cats.syntax.semigroup._
import cats.syntax.apply._
import cats.instances.list._
import cats.instances.int._
import cats.instances.string._

object Checker {
  type Errors = NonEmptyList[String]

  val gt2: Predicate[Errors, Int] =
    Predicate.Pure[Errors, Int](v => Validated.cond(v > 2, v, NonEmptyList.of("musi byc > 2")))

  val even: Predicate[Errors, Int] =
    Predicate.Pure[Errors, Int](v => Validated.cond(v % 2 == 0, v, NonEmptyList.of("musi byc parzyste")))

  val isNotEmpty: Predicate[Errors, String] =
    Predicate.Pure[Errors, String](v => Validated.cond(!v.isEmpty, v, NonEmptyList.of("musi byc nie puste")))

  val c1 = KleisliCheck.checkPred[String](isNotEmpty).map(_.toInt).andThen(KleisliCheck.checkPred(even and gt2))

  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)
  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(error(s"Must be longer than $n characters"), str => str.size > n)
  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(error(s"Must be all alphanumeric characters"), str => str.forall(_.isLetterOrDigit))
  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char"), str => str.contains(char))
  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(error(s"Must contain the character $char only once"), str => str.filter(c => c == char).size == 1)

  def main(args: Array[String]): Unit = {
    import cats.syntax.apply._
    val pp1 = KleisliCheck.checkPred(longerThan(4) and alphanumeric)
    val pp2 = KleisliCheck
      .checkPred(containsOnce('@'))
      .map(_.split('@'))
      .flatMap({ list =>
        KleisliCheck.check(
          z =>
            (KleisliCheck.checkPred(longerThan(0))(list(0)),
             KleisliCheck.checkPred(longerThan(3) and contains('.'))(list(1))).mapN(_ + "@" + _))
      })
//    println(False[String, Int]("errr1").and(False[String, Int]("err2")).apply(1))
//    println(False[String, Int]("errr1").and(True[String, Int](11)).apply(1))
//    println(True[String, Int](11).and(False[String, Int]("errr1")).apply(1))
//    println(True[String, Int](11).or(False[String, Int]("errr1")).apply(1))
//    println(False[String, Int]("err2").or(False[String, Int]("errr1")).apply(1))
    println((gt2 and even).apply(4))
    println((gt2 and even).apply(2))
    println((gt2 or even).apply(3))
    println((gt2 or even).apply(1))
    println((gt2 or even).apply(2))
    println(c1.run("1"))
    println(pp2.apply("kghost@gmail.com"))
    println((containsOnce('@') and longerThan(3)).apply("kghost@"))
  }
}
sealed trait Predicate[E, A] {
  def and(that: Predicate[E, A]): Predicate[E, A] =
    Predicate.And(this, that)
  def or(that: Predicate[E, A]): Predicate[E, A] =
    Predicate.Or(this, that)
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Predicate.Pure(func) =>
        func(a)
      case Predicate.And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)
      case Predicate.Or(left, right) =>
        left(a) match {
          case Valid(a1) => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a2)   => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }

  def run()(implicit se: Semigroup[E]): A => Either[E, A] = { a =>
    this.apply(a).toEither
  }
}
object Predicate {

  def lift[E, A](errors: E, pred: A => Boolean): Predicate[E, A] =
    Predicate.Pure(a => Validated.cond(pred(a), a, errors))

  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

}

object KleisliCheck {
  type Errors = NonEmptyList[String]
  type Result[A] = Either[Errors, A]
  type Check[A, B] = Kleisli[Result, A, B]

  def check[A, B](func: A => Result[B]): Kleisli[Result, A, B] = {
    Kleisli(func)
  }

  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] = {
    Kleisli[Result, A, A](pred.run())
  }
}
