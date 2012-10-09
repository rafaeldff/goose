package net.rafaelferreira.goose
package direct

case class DirectDouble[T](value:T) extends InitializedDouble[T] {
  def value(env:Environment) = Some(value)
}

trait DirectDSL { this: GooseSpecificationDSL =>
  trait DirectDependency[T] { self: GeneralDependency[T] =>
    def ==>(value: T): Assumption[T] = new Assumption[T] {
      def relatedTo = self
      def apply(previous: TestDouble[T], ignored:Environment) = DirectDouble(value)
    }
  }

}