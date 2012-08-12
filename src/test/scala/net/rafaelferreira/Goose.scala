package net.rafaelferreira
import org.specs2
import org.specs2.specification.Fragments
import org.specs2.execute.Result
import org.specs2.specification.Example
import org.specs2.execute.DecoratedResult
import org.specs2.Specification
import org.specs2.matcher.MatchResult

trait Goose {this: Specification =>
  val mocker = new org.specs2.mock.MockitoMocker {} // This is specs2 thin integration layer over Mockito
  
  trait Assumption {
    def init: Unit
    def apply: Unit
  }
  
  class State(assumptions: Seq[Assumption] = Seq()) {
    def assuming(assumption:Assumption) = 
      new State(assumption +: assumptions)
    
    def setup = {
      assumptions.foreach(_.init)
      assumptions.foreach(_.apply)
    }
  }
  
  class When[T](resultExpression: () => T, state: State = new State(), fragments:Fragments = Fragments()) {
    def when(assumption: Assumption): When[T] = copy(newState = state.assuming(assumption)) 
    def and(assumption: Assumption): When[T] = when(assumption)
    
    def but(context: When[T] => When[T]): When[T] = {
      val subWhen = context(this)
      copy(newFragments = fragments add subWhen.results)
    }
    
    def then(expectedExpression: T => MatchResult[Any]): When[T] = {
      state.setup
      
      val got = resultExpression()
      
      val thisExample = eg(expectedExpression(got))
      
      this.copy(newFragments = fragments add thisExample)
    }
    
    def results: Fragments = fragments
    def copy(newResultExpression: () => T = resultExpression, newState: State = new State(), newFragments:Fragments = Fragments()) =
      new When(newResultExpression, newState, newFragments)
    
  }
  
  class Dependency[T: ClassManifest] {
    var result: Option[T] = None
    
    def apply():T = result match {
      case Some(result) => result
      case None => throw new RuntimeException("Dependency wasn't setup properly")
    } 
    
    def ==>(value: T): Assumption = new Assumption {
      def init = result = Some(value)
      def apply = ()
    }
    
    class Stubbing[R](call: T => R) {
      def ==>(r:R) = new Assumption {
        def init = {  
          lazy val mock: T = mocker.mock(implicitly[ClassManifest[T]])
          result = Some(result.getOrElse(mock))
        }
        def apply = result.foreach(mockResult => mocker.when(call(mockResult)).thenReturn(r))
      }
    }
    
    def stub[R](call: T => R) = new Stubbing[R](call) 
   
  }
  
  def dep[T: ClassManifest]: Dependency[T] = new Dependency[T]
  
  def check[T](resultExpression: => T)(c: When[T] => When[T]): Fragments = 
    c(new When(() => resultExpression)).results
}

































