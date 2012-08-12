package net.rafaelferreira

import org.specs2.Specification

class AddressMapperSpec extends Specification with Goose {
  val id = dep[String]
  val database = dep[Database]

  def is = check(new DatabaseBackedAddressMapper(database()).map(id())) {
    _.when(id ==> "123").
      and(database.stub(_.find("addresses", "123")) ==> Some(Map("city" -> "789", "street" -> "999"))).
      and(database.stub(_.find("cities", "789")) ==> Some(Map("name" -> "Curitiba"))).
      and(database.stub(_.find("streets", "999")) ==> Some(Map("name" -> "St. st."))).
      then(_ must_== Some(Address(City("Curitiba"), Street("St. st.")))).but {
        _.when(database.stub(_.find("streets", "999")) ==>  None).
          then(_ must_== None)
      }.but {
        _.when(database.stub(_.find("cities", "789")) ==> None).
          then(_ must beNone)
      }
  }
}