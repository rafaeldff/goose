package net.rafaelferreira.goose
package stubs

import scala.reflect.macros._

import scala.language.experimental.macros

object CallMacro {
  def capture_impl[T: c.AbsTypeTag](c: Context)(methodCall: c.Expr[T => Any]): c.Expr[Call[T]] = {
    import c.universe._
    val (methodTermName, valuesTrees) = methodCall.tree match {
      case Function(_, Select(_, methodTermName)) => (methodTermName, Nil)  
      case Function(_, Apply(Select(_, methodTermName), termValues)) => (methodTermName, termValues)
      case _ => c.abort(c.enclosingPosition, "Expression %s is not a simple method call" format methodCall)
    }
    
    generateCallObject[T](c)(methodTermName, valuesTrees)
  } 
  
  def generateCallObject[T: c.AbsTypeTag](c:Context)(methodTermName: c.Name, valuesTrees: List[c.Tree]) = {
    import c.universe._
    
    val methodNameTree  = Literal(Constant(methodTermName.toString))
    val methodNameExpression = c.Expr[String](methodNameTree)
    
    val valueListTree = 
      Apply(
        Select(
          Select(Select(Select(Ident("scala"), newTermName("collection")), newTermName("immutable")), newTermName("List")),
          newTermName("apply")),
        valuesTrees)
    val valueListExpression = c.Expr[List[Any]](valueListTree)
    
    reify(new Call[T](c.prefix.splice, methodNameExpression.splice, valueListExpression.splice))
  }
}
