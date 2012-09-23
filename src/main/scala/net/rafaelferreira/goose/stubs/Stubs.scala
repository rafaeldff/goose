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
        def apply(previous: TestDouble[T]) = UninitializedDouble
          
          //{
          //val mock = previous match {
            //case None => mocker.mock(manifest)
            //case Some(mock) => mock
          //}
          //mocker.when(call(mock)).thenReturn(r)
          //Some(mock)
        //}
      }
    }

  case class ReturnAssumptionFactory[T](call:Call[T]) {
    def ==>(result:Any):Assumption[T] = new Assumption[T] {
      def relatedTo = call.context.asInstanceOf[GeneralDependency[T]]
      def apply(td:TestDouble[T]): TestDouble[T] = UninitializedDouble
    }
  }
  
  implicit def call2returnAssumptionFactory[T](c:Call[T]) = new ReturnAssumptionFactory[T](c)

  trait StubDependency[T] { self: GeneralDependency[T] =>
    val manifest: ClassTag[T]

    def stub(methodCall: T => Any): Call[T] = macro CallMacro.capture_impl[T]
  }
}
