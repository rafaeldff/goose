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

trait Goose {this: Specification =>
  import scala.collection.immutable.Map
  
  val mocker = new org.specs2.mock.MockitoMocker {}
  
  trait Assumption[T] {
    def relatedTo: Dependency[T]
    def apply(previous:Option[T]):Option[T]
  }
  
  class Dependency[T: ClassManifest] {self =>
    var result: Option[T] = None
    
    def apply():T = result match {
      case Some(result) => result
      case None => throw new RuntimeException("Dependency wasn't setup properly")
    } 
    
    def ==>(value: T): Assumption[T] = new Assumption[T] {
      def relatedTo = self
      def apply(previous:Option[T]) = Some(value)
    }
    
    class Stubbing[R](call: T => R) {
      def ==>(r:R) = new Assumption[T] {
        def relatedTo = self
        def apply(previous:Option[T]) = {
          val mock = previous match {
            case None => mocker.mock(implicitly[ClassManifest[T]])
            case Some(mock) => mock
          }
          mocker.when(call(mock)).thenReturn(r)
          Some(mock)
        }
      }
    }
    
    def stub[R](call: T => R) = new Stubbing[R](call)

    override def toString = "DEP[%s]" format result
  }
  
  type DepGen[T] = Option[T] => Option[T]
  
  class State(assumptions: Map[Dependency[_], Any] = Map().withDefaultValue((_:Any) => None)) {
    def assuming[T](assumption:Assumption[T]) = { 
      val newDep: Dependency[T] = assumption.relatedTo
      val newGen: DepGen[T] = assumption.apply _
      
      val oldGen = getDepGen(newDep)
      
      new State(assumptions + (newDep -> (oldGen andThen newGen)))
    }
    
    private def getDepGen[T](dep: Dependency[T]): DepGen[T] = {
      assumptions(dep).asInstanceOf[DepGen[T]]
    }
    
    def get[T](dep:Dependency[T]): Option[T] = 
      getDepGen(dep)(None)
  }
  
  type ResultThunk[R]  = () => R
  type Then[R] = ResultThunk[R] => Fragment 
  
  class When[R](state: State = new State(), val fragments:Fragments = Fragments())(resultExpression: State => R) {
    
    def when[T](assumption: Assumption[T]): When[R] = copy(newState = state.assuming(assumption)) 
    def and[T](assumption: Assumption[T]): When[R] = when(assumption)
    
    def but(context: When[R] => When[R]): When[R] = {
      val subWhen = context(this)
      copy(newFragments = subWhen.fragments)
    }
    
    def then(expectedExpression: R => MatchResult[Any]): When[R] = {
      val thisExample = eg { expectedExpression(resultExpression(state)) }
      
      this.copy(newFragments = fragments add thisExample)
    }
    
    def results: Fragments = fragments
    
    def copy(newResultExpression: (State => R) = resultExpression, newState: State = state, newFragments:Fragments = fragments) =
      new When(newState, newFragments)(newResultExpression)
    
  }
  
  def dep[T: ClassManifest]: Dependency[T] = new Dependency[T]
  
  def check[T1: ClassManifest, T2: ClassManifest, R](resultExpression: (T1, T2) => R)(c: (Dependency[T1], Dependency[T2]) => When[R] => When[R]): Fragments = {
    val dep1 = dep[T1]
    val dep2 = dep[T2]
    val when = new When[R]()((state:State) => resultExpression(state.get(dep1).get, state.get(dep2).get))
    c(dep1, dep2)(when).results
  }
}





