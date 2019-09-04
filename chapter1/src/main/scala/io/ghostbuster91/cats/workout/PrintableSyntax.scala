package io.ghostbuster91.cats.workout

object PrintableSyntax {
  implicit class PrintableOps[T](t:T) {
    def print(implicit printable: Printable[T]): Unit = {
      println(format)
    }
    def format(implicit printable: Printable[T]) : String = {
      printable.format(t)
    }
  }
}
