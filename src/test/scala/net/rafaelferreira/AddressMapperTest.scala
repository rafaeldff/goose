package net.rafaelferreira

trait Goose {
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
  
  class When[T](resultExpression: => T, state: State = new State()) {
    def when(assumption: Assumption): When[T] = 
      new When(resultExpression, state.assuming(assumption))
    
    def and(assumption: Assumption): When[T] = when(assumption)
    
    def but(context: When[T] => Unit): When[T] = {
      context(this)
      this
    }
    
    def then(expected: T): Unit = {
      state.setup
      
      val got = resultExpression
      if (got == expected)
        println("ok!")
      else
        printf("Fail! expected %s but got %s\n", expected, got)
    }
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
  
  def check[T](resultExpression: => T)(c: When[T] => Unit) = c(new When(resultExpression))
}

































