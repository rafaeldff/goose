package net.rafaelferreira
import org.specs2.mock.MockitoMocker
import org.mockito.stubbing.OngoingStubbing

trait Goose {
  val mocker = new MockitoMocker {}
  
  class Assumption
  
  class Stubbing[T,R](call: T => R, mock: T) {
    def ==>(result:R): Assumption = {
      mocker.when(call(mock)).thenReturn(result)
      new Assumption
    }
  }
  
  class When[T](resultExpression: => T) {
    def apply(f: => Any): When[T] = {f; this}
    def and(f: => Any): When[T] = {f; this}
    def when(f: => Any): When[T] = and(f)
    def but(f: When[T] => Any): When[T] = todo.it
    
    def then(expected: T): Unit = {
      val got = resultExpression
      if (got == expected)
        println("ok!")
      else
        printf("Fail! expected %s but got %s\n", expected, got)
    }
  }
  
  
  class Dummy[R](a: => R) {
    def ==>(value: R):OngoingStubbing[R] = 
      mocker.when(a).thenReturn(a)
    
  }
  
  class Dependency[T: ClassManifest] {
    var result: Option[T] = None
    
    def apply():T = result match {
      case Some(result) => result
      case None => throw new RuntimeException("Dependency wasn't setup properly")
    } 
    
    def ==>(value: T): Assumption = {
      result = Some(value)
      new Assumption
    }
    
    def stub[R](fn: T => R): Stubbing[T,R] = {
      val mock: T = mocker.mock(implicitly[ClassManifest[T]])
      result = Some(mock)
      new Stubbing(fn, mock)
    } 
  }
  
  def dep[T: ClassManifest]: Dependency[T] = new Dependency[T]
  
  def check[T](resultExpression: => T)(c: When[T] => Unit) = c(new When(resultExpression))

  implicit def any2dummy[T](a: => T) = new Dummy[T](a)
}


class AddressMapperTest extends Goose {
  val id = dep[String]
  val database = dep[Database]

  check(new DatabaseBackedAddressMapper(database()).map(id())) {
    _.when(id ==> "123").
      and(database.stub(_.find("addresses", "123")) ==> Some(Map("street" -> "789"))).
      and(database.stub(_.find("cities", "789")) ==> Some(Map("name" -> "Curitiba"))).
      but {
        _.when(database.stub(_.find("streets", "def")) ==> None).
        then(None)
      }.
      but {
        _.when(database.stub(_.find("streets", "def")) ==> Some(Map("name" -> "St. st."))).
        then(Some(Address(City("Curitiba"), Street("St. st."))))
      }
  }
}


































