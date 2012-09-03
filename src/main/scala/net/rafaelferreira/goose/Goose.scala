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

trait GooseStructure {this: Specification =>
  import scala.collection.immutable.Map
  
  val mocker = new org.specs2.mock.MockitoMocker {}
  
  trait Assumption[D] {
    def relatedTo: GeneralDependency[D]
    def apply(previous:Option[D]):Option[D]
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
      def apply(previous:Option[T]) = Some(value)
    }
  }
  
  trait StubDependency[T] {self: GeneralDependency[T] =>
    val manifest: ClassManifest[T]
    class Stubbing[R](call: T => R) {
      def ==>(r:R) = new Assumption[T] {
        def relatedTo = StubDependency.this
        def apply(previous:Option[T]) = {
          val mock = previous match {
            case None => mocker.mock(manifest)
            case Some(mock) => mock
          }
          mocker.when(call(mock)).thenReturn(r)
          Some(mock)
        }
      }
    }
    
    def stub[R](call: T => R) = new Stubbing[R](call)
  }
  
  type DepGen[T] = Option[T] => Option[T]
  
  class State(assumptions: Map[GeneralDependency[_], Any] = Map().withDefaultValue((_:Any) => None)) {
    def assuming[T](assumption:Assumption[T]) = { 
      val newDep: GeneralDependency[T] = assumption.relatedTo
      val newGen: DepGen[T] = assumption.apply _
      
      val oldGen = getDepGen(newDep)
      
      new State(assumptions + (newDep -> (oldGen andThen newGen)))
    }
    
    private def getDepGen[T](dep: GeneralDependency[T]): DepGen[T] = {
      assumptions(dep).asInstanceOf[DepGen[T]]
    }
    
    def get[T](dep: GeneralDependency[T]): Option[T] = 
      getDepGen(dep)(None)
  }
  
  type ResultExpression[R] = Either[String,R]
  
  class When[R](resultExpression: State => ResultExpression[R], state: State = new State(), val fragments:Fragments = Fragments()) {
    
    def when[T](assumption: Assumption[T]): When[R] = copy(newState = state.assuming(assumption)) 
    def and[T](assumption: Assumption[T]): When[R] = when(assumption)
    
    def but(context: When[R] => When[R]): When[R] = {
      val subWhen = context(this)
      copy(newFragments = subWhen.fragments)
    }
    
    def then(expectedExpression: R => MatchResult[Any]): When[R] = {
      val result = resultExpression(state)
      val thisExample = result match {
        case Left(failureMessafe) => eg { Failure(failureMessafe) }
        case Right(resultValue) => eg { (expectedExpression(resultValue)) }
      }
      
      this.copy(newFragments = fragments add thisExample)
    }
    
    def results: Fragments = fragments ^ end
    
    def copy(newResultExpression: (State => ResultExpression[R]) = resultExpression, newState: State = state, newFragments:Fragments = fragments) =
      new When(newResultExpression, newState, newFragments)
    
  }
  
  
  type Dependency[T] <: GeneralDependency[T]
  def dep[T: ClassManifest]: Dependency[T]
  
}


trait Goose extends GooseStructure with CheckingForVariousArities {self: Specification =>
  class ActualDependency[T: ClassManifest] extends GeneralDependency[T] with DirectDependency[T] with StubDependency[T] {self =>
    val manifest = implicitly[ClassManifest[T]]
    override def toString = "DEP[%s]" format result
  }
  
  type Dependency[T] = ActualDependency[T]
  
  def dep[T: ClassManifest]: ActualDependency[T] = new ActualDependency[T]
}
