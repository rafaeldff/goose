package net.rafaelferreira

trait Goose {
  class Assumption
  class When[T] {
    def apply(f: => Assumption): When[T] = todo.it
    def and(f: => Assumption): When[T] = todo.it
    def then(expected: T): Unit = todo.it
  }

  class Dependency[T] {
    def -->(value: T): Assumption = todo.it
  }

  def dep[T]: T with Dependency[T] = todo.it
  def check[T](resultExpression: T)(c: When[T] => Unit) = todo.it

  implicit def any2dummy(a: Any) = new {
    def ==>(value: Any): Assumption = todo.it
  }
}


class AddressMapperTest extends Goose {
  val id = dep[String]
  val database = dep[Database]

  check(new DatabaseBackedAddressMapper(database).map(id)) { when =>
    when(id --> "abc").and(database.find("abc") ==> None).then(None)
  }

}