package net.rafaelferreira.goose
package direct

case class DirectDouble[T](value:T) extends InitializedDouble[T]

trait Direct { this: GooseStructure =>
  trait DirectDependency[T] { self: GeneralDependency[T] =>
    def ==>(value: T): Assumption[T] = new Assumption[T] {
      def relatedTo = self
      def apply(previous: TestDouble[T]) = DirectDouble(value)
    }
  }

}