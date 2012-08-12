package net.rafaelferreira

case class Street(streetName: String)
case class City(name:String)
case class Address(city:City, street:Street)

trait AddressMapper {
  def map(id:String): Option[Address]
}

trait Database {
  def find(table:String, id:String): Option[Map[String,String]]
}

object todo {
  def it[T]:T = sys.error("todo")
}

class DatabaseBackedAddressMapper(db:Database) extends AddressMapper {
  override def map(id:String): Option[Address] = { 
    printf("id is %s, db.find is %s\n", 
        id, db.find("a" , id));
    Some(Address(City("Curitiba"),Street("xyz")))
  }
}