package net.rafaelferreira
package goose

trait TestDouble[+T]
object UninitializedDouble extends TestDouble[Nothing]
trait InitializedDouble[T] extends TestDouble[T] {
  def value: T
}
object InitializedDouble {
  def unapply[T](double: InitializedDouble[T]) = Some(double.value)
}

trait GeneralDependency[T] {
}

trait Assumption[D] {
  def relatedTo: GeneralDependency[D]
  def apply(double: TestDouble[D], env:Environment): TestDouble[D]
}

class Environment(bindings: Map[GeneralDependency[_], TestDouble[_]] = Map().withDefaultValue(UninitializedDouble)) {
  def assuming[T](assumption: Assumption[T]) = {
    val oldDouble = get(assumption.relatedTo)
    val newDouble = assumption(oldDouble, this)

    new Environment(bindings + ((assumption.relatedTo) -> newDouble))
  }

  def get[T](dep: GeneralDependency[T]): TestDouble[T] =
    bindings(dep).asInstanceOf[TestDouble[T]]

  override def toString = "State(" + bindings.toString + ")"
}