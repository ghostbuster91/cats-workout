package io.ghostbuster91.cats.workout

import cats.Eval

object SafeFold {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

  def safeFoldRight[A, B](as: List[A], acc: B)(fn: (A, Eval[B]) => Eval[B]): Eval[B] = {
    val eval = as match {
      case head :: tail => Eval.defer(fn(head, safeFoldRight(tail, acc)(fn)))
      case Nil =>
        Eval.now(acc)
    }
    eval
  }
}
