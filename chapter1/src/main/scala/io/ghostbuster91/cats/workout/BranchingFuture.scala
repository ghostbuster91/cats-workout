package io.ghostbuster91.cats.workout

object BranchingFuture {
  def main(args: Array[String]): Unit = {
    import Tree._
    import TreeImplicits._
    import TreeSyntax._
    val result = for {
      a <- branch(leaf(100), leaf(200))
      b <- branch(leaf(a - 10), leaf(a + 10))
      c <- branch(leaf(b - 1), leaf(b + 1))
    } yield c
    println(result)
  }
}

object TreeSyntax {

  implicit class TreeOpts[T](t: Tree[T]) {
    def map[TT](f: T => TT)(implicit m: MyMonad[Tree]): Tree[TT] = {
      m.map(t)(f)
    }

    def flatMap[TT](f: T => Tree[TT])(implicit m: MyMonad[Tree]): Tree[TT] = {
      m.flatMap(t)(f)
    }
  }

}

object TreeImplicits {
  implicit val mTree: MyMonad[Tree] = new MyMonad[Tree] {
    override def pure[A](a: A): Tree[A] = Tree.leaf(a)

    override def flatMap[A, B](value: Tree[A])(func: A => Tree[B]): Tree[B] = {
      value match {
        case Branch(l, r) => Branch(flatMap(l)(func), flatMap(r)(func))
        case Leaf(v) => func(v)
      }
    }
  }
}
