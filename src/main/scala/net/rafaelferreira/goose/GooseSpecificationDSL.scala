package net.rafaelferreira
package goose

import scala.reflect.ClassTag

import org.specs2.Specification
import org.specs2.execute.Failure
import org.specs2.matcher.MatchResult
import org.specs2.specification.Fragments

trait GooseSpecificationDSL extends Specification {
  
  type ResultExpression[R] = Environment => Either[String,R]
  
  class When[R](resultExpression: ResultExpression[R], environment: Environment = new Environment(), val fragments:Fragments = Fragments()) {
    def when[T](assumption: Assumption[T]): When[R] = copy(newState = environment.assuming(assumption)) 
    def and[T](assumption: Assumption[T]): When[R] = when(assumption)
    
    def but(context: When[R] => When[R]): When[R] = {
      val subWhen = context(this)
      copy(newFragments = subWhen.fragments)
    }
    
    def then(expectedExpression: R => MatchResult[Any]): When[R] = {
      val result = resultExpression(environment)
      val thisExample = result match {
        case Left(failureMessafe) => eg { Failure(failureMessafe) }
        case Right(resultValue) => eg { (expectedExpression(resultValue)) }
      }
      
      this.copy(newFragments = fragments add thisExample)
    }
    
    def results: Fragments = fragments ^ end
    
    private def copy(newResultExpression: ResultExpression[R] = resultExpression, newState: Environment = environment, newFragments:Fragments = fragments) =
      new When(newResultExpression, newState, newFragments)
    
  }
  
  type Dependency[T] <: GeneralDependency[T]
  def newDependency[T: ClassTag](name:String): Dependency[T]
}