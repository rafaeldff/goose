package net.rafaelferreira

case class Street(streetName: String)
case class Address(number:Int, street:Street)

trait AddressMapper {
  def map(id:String): Option[Address]
}

trait Database {
  def find(id:String): Option[Map[String,String]]
}

object todo {
  def it[T]:T = sys.error("todo")
}

class DatabaseBackedAddressMapper(db:Database) extends AddressMapper {
  override def map(id:String): Option[Address] = todo.it 
}