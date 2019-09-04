package io.ghostbuster91.cats.workout

import cats.Eval
import cats.instances.int._
import cats.instances.option._
import cats.kernel.{Monoid, Semigroup}
import io.ghostbuster91.cats.workout.PrintableInstances._
import io.ghostbuster91.cats.workout.PrintableSyntax._

object Main {
  def main(args: Array[String]): Unit = {
    val cat = Cat2("kasper", 11, "blue")
    Printable.print(cat)
    cat.print

    println(SuperAdder.add(List(1, 2, 3, 4)))
    println(SuperAdder.add(List(Some(1), None)))
    println(SuperAdder.add(List(Order(1,2), Order(2,3))))
    import cats.syntax.functor._
    println((Branch(Leaf(20), Leaf(10)): Tree[Int]).map(_ * 2))
    Box("hello world").print
    // res5: String = "hello world"
    Box(true).print


    val ans = for {
      a <- Eval.now { println("Calculating A"); 40 }
      b <- Eval.now { println("Calculating B"); 2 }
    } yield {
      println("Adding A and B")
      a + b
    }
    ans.value
    ans.value
    val ints = List.fill(1000000) { 1 }
    println(ints.size)
//    SafeFold.foldRight[Int,Int](ints,0)({(acc:Int, item:Int)=> acc + item})
    println(SafeFold.safeFoldRight[Int,Int](ints,0){(item:Int,acc:Eval[Int])=>acc.map(_ + item)}.value)
  }
}

case class Order(totalCost: Double, quantity: Double)

object Order {
  implicit val semigroup : Monoid[Order] = new Monoid[Order] {
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost,x.quantity + y.quantity)

    override def empty: Order = Order(0,0)
  }
}

final case class Box[A](value: A)

object Box {
  implicit def printable[A:Printable]:Printable[Box[A]] = new Printable[Box[A]] {
    override def format(t: Box[A]): String = {
      implicitly[Printable[A]].contramap[Box[A]](_.value).format(t)
    }
  }
}
