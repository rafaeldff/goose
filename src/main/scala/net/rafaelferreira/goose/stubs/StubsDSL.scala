package net.rafaelferreira.goose
package stubs

import scala.reflect.ClassTag

import scala.language.experimental.macros

trait StubsDSL { this: GooseSpecificationDSL =>
  class ReturnAssumptionFactory[T: ClassTag](call: Call[T]) {
    def ==>(dependency: GeneralDependency[_]) = new Assumption[T] {
      println("GO!")
      def relatedTo = call.context.asInstanceOf[GeneralDependency[T]]
      def apply(testDouble: TestDouble[T], env: Environment):TestDouble[T] = {
        val result = env.get(dependency) 
        (testDouble, result) match {
          case (UninitializedDouble, InitializedDouble(value:AnyRef)) => StubDouble(Vector(Expectation(call, value)))
          case (double: StubDouble[T], InitializedDouble(value:AnyRef)) => double expecting Expectation(call, value)
          case _ => sys.error("Cannot mix stub with non-stub expectations.")
        }
      }
    }

    def ==>(result: AnyRef): Assumption[T] = new Assumption[T] {
      def relatedTo = call.context.asInstanceOf[GeneralDependency[T]]
      def apply(testDouble: TestDouble[T], env: Environment):TestDouble[T] =
        testDouble match {
          case UninitializedDouble => StubDouble(Vector(Expectation(call, result)))
          case double: StubDouble[T] => double expecting Expectation(call, result)
          case _ => sys.error("Cannot mix stub with non-stub expectations.")
        }
    }
  }

  implicit def call2returnAssumptionFactory[T: ClassTag](c: Call[T]): ReturnAssumptionFactory[T] =
    new ReturnAssumptionFactory[T](c)

  trait StubDependency[T] { self: GeneralDependency[T] =>
    def stub(methodCall: T => Any): Call[T] = macro CallMacro.capture_impl[T]
  }
}