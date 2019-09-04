package io.ghostbuster91.cats.workout
import cats.Monoid
import cats.implicits._

object SuperAdder {
  def add[T](items: List[T])(implicit monoid: Monoid[T]): T = {
    items.foldLeft(monoid.empty)(_ |+| _)
  }
}

