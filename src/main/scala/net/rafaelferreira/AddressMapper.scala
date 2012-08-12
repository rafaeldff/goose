package net.rafaelferreira

case class Street(streetName: String)
case class City(name:String)
case class Address(city:City, street:Street)

trait AddressMapper {
  def mapStreet(id:String): Option[Street]
  def mapCity(id:String): Option[City]
  def mapAddress(id:String): Option[Address]
}

trait Database {
  def find(table:String, id:String): Option[Map[String,String]]
}

object todo {
  def it[T]:T = sys.error("todo")
}

class DatabaseBackedAddressMapper(db:Database) extends AddressMapper {
  def mapStreet(id:String): Option[Street] = 
    for (record <- db.find("streets", id))
      yield Street(record("name"))  
    
  def mapCity(id:String): Option[City] =
    for (record <- db.find("cities", id))
      yield City(record("name "))

  override def mapAddress(id:String): Option[Address] = { 
    for (address <- db.find("addresses", id);
         city <- db.find("cities", address("city"));
         street <- db.find("streets", address("street")))
    yield Address(City(city("name")),Street(street("name")))
  }
}

trait Foo {def foo: String}