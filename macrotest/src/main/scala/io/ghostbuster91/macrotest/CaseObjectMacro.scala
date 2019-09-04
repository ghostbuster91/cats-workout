package io.ghostbuster91.macrotest

import scala.reflect.macros.blackbox

object CaseObjectMacro {

  def oneOfMacro[E: c.WeakTypeTag](c: blackbox.Context): c.Expr[Set[E]] = {
    import c.universe._
    val symbol = weakTypeOf[E].typeSymbol.asClass
    if (!symbol.isClass || !symbol.isSealed) {
      c.abort(c.enclosingPosition, "Can only enumerate values of a sealed trait or class.")
    } else {
      val subclasses = symbol.knownDirectSubclasses
      if (!subclasses.forall(_.isModuleClass)) {
        c.abort(c.enclosingPosition, "All children must be objects.")
      } else {
        val instances = subclasses.map(x => Ident(x.asInstanceOf[scala.reflect.internal.Symbols#Symbol].sourceModule.asInstanceOf[Symbol]))
        c.Expr[Set[E]](q"$instances")
      }
    }
  }

  def gen[E] = macro oneOfMacro[E]

}
