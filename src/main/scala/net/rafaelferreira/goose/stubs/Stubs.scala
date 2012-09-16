package net.rafaelferreira.goose
package stubs

import org.specs2.Specification
import scala.reflect.ClassTag

import scala.language.experimental.macros

trait Stubs { this: GooseStructure =>
  private[Stubs] val mocker = new org.specs2.mock.MockitoMocker {}
  
  class Stubbing[T,R](dependency: GeneralDependency[T])(call: T => R) {
      def ==>(r: R) = new Assumption[T] {
        def relatedTo = dependency
        def apply(previous: Option[T]) = {
          val mock = previous match {
            case None => mocker.mock(manifest)
            case Some(mock) => mock
          }
          mocker.when(call(mock)).thenReturn(r)
          Some(mock)
        }
      }
    }

  case class ReturnAssumptionFactory(call:Call) {
    def ==>(result:Any) = ???
  }
  
  implicit def call2returnAssumptionFactory(c:Call) = new ReturnAssumptionFactory(c)

  trait StubDependency[T] { self: GeneralDependency[T] =>
    val manifest: ClassTag[T]

    def stub(methodCall: T => Any): Call = macro CallMacro.capture_impl[T]
  }
}