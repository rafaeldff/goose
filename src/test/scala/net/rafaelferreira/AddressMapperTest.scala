package net.rafaelferreira
import org.specs2.mock.MockitoMocker
import org.mockito.stubbing.OngoingStubbing

trait Goose {
  val mocker = new MockitoMocker {}
  
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
    
    def stub[R](call: T => R, r:R) = new Assumption {
      def init = {  
        lazy val mock: T = mocker.mock(implicitly[ClassManifest[T]])
        result = Some(result.getOrElse(mock))
      }
      def apply = result.foreach(mockResult => mocker.when(call(mockResult)).thenReturn(r))
    }
   
  }
  
  def dep[T: ClassManifest]: Dependency[T] = new Dependency[T]
  
  def check[T](resultExpression: => T)(c: When[T] => Unit) = c(new When(resultExpression))
}


class AddressMapperTest extends Goose {
  val id = dep[String]
  val database = dep[Database]

  check(new DatabaseBackedAddressMapper(database()).map(id())) {
    _.when(id ==> "123").
      and(database.stub(_.find("addresses", "123"), Some(Map("city" -> "789", "street" -> "999")))).
      and(database.stub(_.find("cities", "789"), Some(Map("name" -> "Curitiba")))).
      but {
        _.when(database.stub(_.find("streets", "999"), None)).
        then(None)
      }.
      but {
        _.when(database.stub(_.find("streets", "999"), Some(Map("name" -> "St. st.")))).
        then(Some(Address(City("Curitiba"), Street("St. st."))))
      }
  }
}


































