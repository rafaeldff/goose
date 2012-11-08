package net.rafaelferreira.goose
package stubs

import scala.reflect.macros._

import scala.language.experimental.macros

object CallMacro {
  def capture_impl[T: c.WeakTypeTag](c: Context)(methodCall: c.Expr[T => Any]): c.Expr[Call[T]] = {
    import c.universe._
    val (methodTermName, valuesTrees) = methodCall.tree match {
      case Function(_, Select(_, methodTermName)) => (methodTermName, Nil)  
      case Function(_, Apply(Select(_, methodTermName), termValues)) => (methodTermName, termValues)
      case _ => c.abort(c.enclosingPosition, "Expression %s is not a simple method call" format methodCall)
    }
    
    generateCallObject[T](c)(methodTermName, valuesTrees)
  } 
  
  def generateCallObject[T: c.WeakTypeTag](c:Context)(methodTermName: c.Name, valuesTrees: List[c.Tree]) = {
    import c.universe._
    
    val methodNameTree  = Literal(Constant(methodTermName.toString))
    val methodNameExpression = c.Expr[String](methodNameTree)
    
    val valueListTree = 
      Apply(
        Select(
          Select(Select(Select(Ident("scala"), newTermName("collection")), newTermName("immutable")), newTermName("List")),
          newTermName("apply")),
        filterArguments(c)(valuesTrees))
    val valueListExpression = c.Expr[List[Any]](valueListTree)
    
    reify(new Call[T](c.prefix.splice, methodNameExpression.splice, valueListExpression.splice))
  }
  
  def filterArguments(c:Context)(arguments:List[c.Tree]): List[c.Tree] = { 
    import c.universe._
    
    val unwrapDependencyImplicitMethod = newTermName("unwrapDependency")
    
    arguments.map {
      case Apply(TypeApply(Select(_, methodName), _), List(dependencyArgument)) if methodName == unwrapDependencyImplicitMethod =>  
        //c.info(c.enclosingPosition, "Implicit dependency found", false)
        dependencyArgument
      case other => 
        other 
    }
  }
  
}
