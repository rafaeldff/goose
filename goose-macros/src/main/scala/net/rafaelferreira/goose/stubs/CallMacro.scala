package net.rafaelferreira.goose
package stubs

import scala.reflect.macros._

import scala.language.experimental.macros

object CallMacro {
  def capture_impl[T: c.AbsTypeTag](c: Context)(methodCall: c.Expr[T => Any]): c.Expr[Call] = {
    import c.universe._
    val Function(_, Select(_, methodTermName)) = methodCall.tree  
    val methodNameTree: Tree = Literal(Constant(methodTermName.toString))
    val methodNameExpression = c.Expr[String](methodNameTree)
    reify( new net.rafaelferreira.goose.stubs.Call(methodNameExpression.splice, Nil) )
  }   
}
