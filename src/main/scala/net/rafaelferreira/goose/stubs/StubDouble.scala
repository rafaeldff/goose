package net.rafaelferreira.goose
package stubs

import scala.language.experimental.macros
import scala.reflect.ClassTag
import org.specs2.matcher.Matcher

import ProxyFactory._ 


case class StubDouble[T: ClassTag](expectations: Seq[Expectation[T]] = Vector()) extends InitializedDouble[T] {
  
  def expecting[R](expectation:Expectation[T]):StubDouble[T] = 
    copy(expectations = expectation +: expectations) 
  
  def value(env:Environment) = 
    Some(ProxyFactory { invocation =>
      val expectedResult = expectations.find(_ appliesTo invocation).map(_.result)
      val result = expectedResult match {
        case Some(dependency:GeneralDependency[AnyRef]) => env.valueFor[AnyRef](dependency)
        case anything:AnyRef => anything
      }
      result.getOrElse(default)
    })
  
  def default = null
}

case class Expectation[T:ClassTag](call: Call[T], resultObject: AnyRef) {
  val methodCalled = call.method
  def appliesTo(invocation: Invocation): Boolean = {
    def methodMatches = call.method == invocation.method
    def aritiesMatch = call.args.size == invocation.arguments.size 
    def argumentsMatch = call.args.zip(invocation.arguments).forall {
      case (expected: Matcher[Any] , actual) => expected.test(actual)  
      case (literalExpected, actual) => literalExpected == actual
    } 
    methodMatches && aritiesMatch && argumentsMatch
  }
  
  def result:AnyRef = resultObject 
}
