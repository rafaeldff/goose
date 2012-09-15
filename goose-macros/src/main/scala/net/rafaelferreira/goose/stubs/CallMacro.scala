package net.rafaelferreira.goose
package stubs

import scala.reflect.macros._

import scala.language.experimental.macros

object CallMacro {
  def capture_impl[T: c.AbsTypeTag](c: Context)(methodCall: c.Expr[T => Any]): c.Expr[Call] = {
    import c.universe._
    val (methodTermName, values) = methodCall.tree match {
      case Function(_, Select(_, methodTermName)) => (methodTermName, None)  
      case Function(_, Apply(Select(_, methodTermName), termValues)) => (methodTermName, Some(termValues))
      case _ => c.abort(c.enclosingPosition, "Expression %s is not a simple method call" format methodCall)
    }
    val methodNameTree: Tree = Literal(Constant(methodTermName.toString))
    val methodNameExpression = c.Expr[String](methodNameTree)
    
    values match {
      case None => reify( new Call(methodNameExpression.splice, Nil) )
      case Some(valuesTrees) =>
        val valueListTree = 
          Apply(
            Select(
              Select(Select(Select(Ident("scala"), newTermName("collection")), newTermName("immutable")), newTermName("List")),
              newTermName("apply")),
            valuesTrees)
                  
        val valueListExpression = c.Expr[List[Any]](valueListTree)
        reify(new Call(methodNameExpression.splice, valueListExpression.splice))
    }
  }   
}


/*
Function(List(ValDef(Modifiers(PARAM | SYNTHETIC), newTermName("x$1"), Ident(Foo), EmptyTree)), 
Apply(Select(Typed(Ident(newTermName("x$1")), Ident(Foo)), newTermName("oneArg")), List(Literal(Constant(9)))))

*/