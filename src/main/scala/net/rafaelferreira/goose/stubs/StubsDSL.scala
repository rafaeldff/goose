package net.rafaelferreira.goose
package stubs

import scala.reflect.ClassTag

import scala.language.experimental.macros

trait StubsDSL { this: GooseSpecificationDSL =>
  class ReturnAssumptionFactory[T: ClassTag](call:Call[T]) {
    def ==>(result:AnyRef):Assumption[T] = new Assumption[T] {
      def relatedTo = call.context.asInstanceOf[GeneralDependency[T]]
      def apply(testDouble: TestDouble[T]): TestDouble[T] =
        testDouble match {
        case UninitializedDouble => StubDouble(Vector(Expectation(call,result)))
        case StubDouble(expectations) => StubDouble(expectations :+ Expectation(call, result))
        case _ => sys.error("Cannot mix stub with non-stub expectations.")
      }
    }
  }
  
  implicit def call2returnAssumptionFactory[T: ClassTag](c:Call[T]):ReturnAssumptionFactory[T] =
    new ReturnAssumptionFactory[T](c)

  trait StubDependency[T] { self: GeneralDependency[T] =>
    def stub(methodCall: T => Any): Call[T] = macro CallMacro.capture_impl[T]
  }
}