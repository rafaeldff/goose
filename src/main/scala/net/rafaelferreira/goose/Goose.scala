package net.rafaelferreira
package goose

import org.specs2
import org.specs2.specification.Fragments
import org.specs2.execute.Result
import org.specs2.specification.Example
import org.specs2.execute.DecoratedResult
import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.Fragment
import org.specs2.execute.Failure
import scala.reflect.ClassTag

trait GooseStructure {this: Specification =>
  import scala.collection.immutable.Map
  
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

trait Goose extends GooseStructure with CheckingForVariousArities with stubs.Stubs with direct.Direct {self: Specification =>
  class ActualDependency[T: ClassTag](name:String) extends GeneralDependency[T] with DirectDependency[T] with StubDependency[T] {self =>
    override def toString = "DEP[%s]" format name
  }
  
  type Dependency[T] = ActualDependency[T]
  
  def newDependency[T: ClassTag](name:String): ActualDependency[T] = new ActualDependency[T](name)
}