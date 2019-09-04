package io.ghostbuster91.cats.workout

trait Printable[A] { outer=>
  def format(t: A): String

  def contramap[B](func: B=>A):Printable[B] = new Printable[B] {
    override def format(t: B): String = {
      outer.format(func(t))
    }
  }
}

object PrintableInstances {
  implicit val printableInt: Printable[Int] = (t: Int) => t.toString
  implicit val printableString: Printable[String] = (t: String) => s"'$t'"
  implicit val printableCat: Printable[Cat2] = new Printable[Cat2] {
    override def format(t: Cat2): String = {
      val stringPrintable = implicitly[Printable[String]]
      val intPrintable = implicitly[Printable[Int]]
      s"NAME is ${stringPrintable.format(t.name)} AGE ${intPrintable.format(t.age)} COLOR ${stringPrintable.format(t.color)}"
    }
  }
  implicit val printableBoolean: Printable[Boolean] = new Printable[Boolean] {
    override def format(t: Boolean): String = if(t) "yes" else "no"
  }
}

object Printable {
  def format[T](t: T)(implicit printable: Printable[T]): String = {
    printable.format(t)
  }

  def print[T](t: T)(implicit printable: Printable[T]): Unit = {
    println(printable.format(t))
  }
}
final case class Cat2(name:String, age: Int, color:String)
