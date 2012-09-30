package net.rafaelferreira.goose
package stubs

import scala.language.experimental.macros
import scala.reflect.ClassTag

case class StubDouble[T: ClassTag](expectations: Seq[Expectation[T]] = Vector()) extends InitializedDouble[T] {
  import ProxyFactory._ 
  
  lazy val results = 
    expectations.foldLeft(Map[String, AnyRef]()) {(map, expectation) =>
      map + (expectation.methodCalled -> expectation.result) 
    }
  
  def expecting[R](expectation:Expectation[T]):StubDouble[T] = 
    copy(expectations = expectations :+ expectation) 
  
  def value: T = 
    ProxyFactory { (obj:Subject, method:Method, args: Arguments) =>
      results(method.getName)
    }
}

case class Expectation[T:ClassTag](call: Call[T], result: AnyRef) {
  val methodCalled = call.method
  
}
