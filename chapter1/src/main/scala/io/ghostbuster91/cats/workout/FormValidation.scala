package io.ghostbuster91.cats.workout

import cats.Semigroupal
import cats.data.{NonEmptyList, Validated}
import cats.syntax.either._
import cats.syntax.semigroupal._
import cats.syntax.apply._

import scala.util.Try
object FormValidation {
  type AllErrorsOr[A] = Validated[NonEmptyList[String], A]
  case class User(name: String, age: Int)

  def main(args: Array[String]) = {
    println(readUser(Map("name" -> "Kasper", "age" -> "12")))
    println(readUser(Map("name" -> "", "age" -> "-12")))
  }

  def readUser(input: Map[String, String]): Validated[NonEmptyList[String], User] = {
    (readName(input).toValidated, readAge(input).toValidated).mapN(User.apply)
  }

  def readName(input: Map[String, String]): Either[NonEmptyList[String], String] = {
    for {
      v <- getValue(input, "name")
      _ <- nonBlank(v)
    } yield v
  }

  def readAge(input: Map[String, String]): Either[NonEmptyList[String], Int] = {
    for {
      v <- getValue(input, "age")
      parsed <- parseInt(v)
      _ <- nonNegative(parsed)
    } yield parsed
  }

  def getValue(input: Map[String, String], key: String): Either[NonEmptyList[String], String] = {
    Either.fromOption(input.get(key), NonEmptyList.of(s"$key not found!"))
  }

  def parseInt(text: String): Either[NonEmptyList[String], Int] = {
    Try(text.toInt).toEither.leftMap(t => NonEmptyList.of(t.getMessage))
  }

  def nonBlank(text: String): Either[NonEmptyList[String], String] = {
    Either.cond(!text.isEmpty, text, NonEmptyList.of("text is empty"))
  }

  def nonNegative(int: Int): Either[NonEmptyList[String], Int] = {
    Either.cond(int >= 0, int, NonEmptyList.of("Is negative!"))
  }
}
