package io.ghostbuster91.cats.workout.part4

import cats.syntax.semigroup._
import cats.instances.map._
import cats.instances.int._
import cats.kernel.CommutativeMonoid
import cats.instances.set._
import cats.instances.list._
import cats.syntax.foldable._
import Utils._

case class GenericCounter[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)(implicit cm: CommutativeMonoid[A]): GenericCounter[A] = {
    copy(counters |+| Map(machine -> amount))
  }

  def merge(that: GenericCounter[A])(implicit bsl: BoundedSemiLattice[A]): GenericCounter[A] = {
    copy(that.counters |+| counters)
  }

  def total(implicit cm: CommutativeMonoid[A]): A = counters.values.toList.combineAll
}

//case class GCounter(counters: Map[String, Int]) {
//  def increment(machine: String, amount: Int): GCounter = {
//    copy(counters |+| Map(machine -> amount))
//  }
//
//  def merge(that: GCounter): GCounter = {
//    copy(that.counters ++ counters.map { case (k, v) => k -> (v max that.counters.getOrElse(k, 0)) })
//  }
//
//  def total: Int = counters.values.sum
//}

case class GSet[A](counters: Map[String, Set[A]]) {
  def increment(machine: String, amount: Set[A]): GSet[A] = {
    copy(counters |+| Map(machine -> amount))
  }

  def merge(that: GSet[A]): GSet[A] = {
    copy(that.counters ++ counters.map {
      case (k, v) =>
        k -> (if (v.size > that.counters.getOrElse(k, Set.empty[A]).size) {
                v
              } else {
                that.counters.getOrElse(k, Set.empty[A])
              })
    })
  }

  def total: Set[A] = counters.values.reduce((a, b) => a ++ b)
}

object GcounterLauncher {
  def main(args: Array[String]): Unit = {
    val first = GCounter[Map, String, Int]
      .increment(Map.empty)("a", 1)
      .run(GCounter[Map, String, Int].increment(_)("a", 1))

    val second = GCounter[Map, String, Int]
      .increment(Map.empty)("b", 2)
      .run(GCounter[Map, String, Int].increment(_)("b", 1))

    println(
      GCounter[Map, String, Int]
        .merge(first, second)
        .run(GCounter[Map, String, Int].merge(_, second))
        .run(GCounter[Map, String, Int].total(_)))
    gset
  }

  def gset: Unit = {
    val first = GenericCounter[Set[Int]](Map.empty)
      .increment("a", Set(1))
      .increment("a", Set(2))

    val second = GenericCounter[Set[Int]](Map.empty)
      .increment("b", Set(3, 4))
      .increment("b", Set(5))
    println(first.merge(second).merge(second).total)
  }
}

trait BoundedSemiLattice[A] extends CommutativeMonoid[A]

object BoundedSemiLattice {
  implicit val intInstance: BoundedSemiLattice[Int] = new BoundedSemiLatticeInt
  implicit def setInstance[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLatticeSet[A]
}

class BoundedSemiLatticeInt extends BoundedSemiLattice[Int] {
  override def empty: Int = 0
  override def combine(x: Int, y: Int): Int = x max y
}

class BoundedSemiLatticeSet[A] extends BoundedSemiLattice[Set[A]] {
  override def empty: Set[A] = Set.empty
  override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
}

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) = counter

  implicit def mapInstance[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
    override def increment(f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
      f |+| Map(k -> v)
    }

    override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] = {
      f1 |+| f2
    }

    override def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V = {
      f.values.toList.combineAll
    }
  }
}

object Utils {
  implicit class AnyUtils[T](v: T) {
    def run[R](f: T => R): R = f(v)
  }
}
