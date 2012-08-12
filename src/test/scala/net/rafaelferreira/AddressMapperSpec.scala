package net.rafaelferreira

class AddressMapperSpec extends Goose {
  val id = dep[String]
  val database = dep[Database]

  check(new DatabaseBackedAddressMapper(database()).map(id())) {
    _.when(id ==> "123").
      and(database.stub(_.find("addresses", "123")) ==> Some(Map("city" -> "789", "street" -> "999"))).
      and(database.stub(_.find("cities", "789")) ==> Some(Map("name" -> "Curitiba"))).
      but {
        _.when(database.stub(_.find("streets", "999")) ==>  None).
        then(None)
      }.
      but {
        _.when(database.stub(_.find("streets", "999")) ==> Some(Map("name" -> "St. st."))).
        then(Some(Address(City("Curitiba"), Street("St. st."))))
      }
  }
}