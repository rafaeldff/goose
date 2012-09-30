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

trait TestDouble[+T]
object UninitializedDouble extends TestDouble[Nothing]
trait InitializedDouble[T] extends TestDouble[T] {
  def value: T
}
object InitializedDouble {
  def unapply[T](double: InitializedDouble[T]) = Some(double.value)
}

trait GooseStructure {this: Specification =>
  import scala.collection.immutable.Map
  
  trait Assumption[D] {
    def relatedTo: GeneralDependency[D]
    def apply(double:TestDouble[D]):TestDouble[D]
  }

  trait GeneralDependency[T] {
    var result: Option[T] = None
    
    def apply():T = result match {
      case Some(result) => result
      case None => throw new RuntimeException("Dependency wasn't setup properly")
    } 
  }
  
  trait DirectDependency[T] {self: GeneralDependency[T] =>
    def ==>(value: T): Assumption[T] = new Assumption[T] {
      def relatedTo = self
      def apply(previous:TestDouble[T]) = DirectDouble(value)
    }
  }
  
  case class DirectDouble[T](value:T) extends InitializedDouble[T] 
  
  class Environment(bindings: Map[GeneralDependency[_], TestDouble[_]] = Map().withDefaultValue(UninitializedDouble)) {
    def assuming[T](assumption:Assumption[T]) = {
      val oldDouble = get(assumption.relatedTo)
      val newDouble  = assumption(oldDouble)
      
      new Environment(bindings + ((assumption.relatedTo) -> newDouble))
    }

    def get[T](dep: GeneralDependency[T]): TestDouble[T] = 
      bindings(dep).asInstanceOf[TestDouble[T]]
    
    override def toString = "State("+bindings.toString+")"
  }
  
  type ResultExpression[R] = Either[String,R]
  
  class When[R](resultExpression: Environment => ResultExpression[R], environment: Environment = new Environment(), val fragments:Fragments = Fragments()) {
    
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
    
    def copy(newResultExpression: (Environment => ResultExpression[R]) = resultExpression, newState: Environment = environment, newFragments:Fragments = fragments) =
      new When(newResultExpression, newState, newFragments)
    
  }
  
  type Dependency[T] <: GeneralDependency[T]
  
  def dep[T: ClassTag](name:String): Dependency[T]
}

trait Goose extends GooseStructure with CheckingForVariousArities with stubs.Stubs {self: Specification =>
  class ActualDependency[T: ClassTag](name:String) extends GeneralDependency[T] with DirectDependency[T] with StubDependency[T] {self =>
    val manifest = implicitly[ClassTag[T]]
    override def toString = "DEP[%s]" format name
  }
  
  type Dependency[T] = ActualDependency[T]
  
  def dep[T: ClassTag](name:String): ActualDependency[T] = new ActualDependency[T](name)
}