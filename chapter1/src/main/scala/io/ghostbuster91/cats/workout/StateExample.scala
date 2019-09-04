package io.ghostbuster91.cats.workout

import cats.data.State

import scala.util.{Failure, Success, Try}

object StateExample {
  type CalcState[A] = State[List[Int], A]

  def main(args: Array[String]): Unit = {
    val v = evalOne("42").runA(Nil).value
    val result = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      _ <- evalOne("+")
      _ <- evalOne("3")
      a <- evalOne("*")
    } yield a
    println(result.runA(Nil).value)

    val program = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans
    println(program.runA(Nil).value)

    println(evalInput("1 2 + 3 *").runA(Nil).value)
  }

  def evalOne(sym: String): CalcState[Int] = {
    val number = raw"(\d+)".r
    State[List[Int], Int] { num =>
      sym match {
        case number(v) =>
          (v.toInt +: num, v.toInt)
        case _ => num match {
          case first :: second :: rest =>
            val result = sym match {
              case "*" => first * second
              case "+" => first + second
              case _ => ???
            }
            (result +: rest, result)
          case _ => ???
        }
      }
    }
  }
  def evalAll(input: List[String]): CalcState[Int] = {
//    input match {
//      case h :: t => evalOne(h).flatMap(_=>evalAll(t))
//    }
    input.map(evalOne).reduce((a,b)=> a.flatMap(_=>b))
  }

  def evalInput(string: String) :CalcState[Int] = {
    evalAll(string.split(' ').toList)
  }
}
