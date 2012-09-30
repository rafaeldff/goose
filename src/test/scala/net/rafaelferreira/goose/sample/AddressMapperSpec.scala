package net.rafaelferreira.goose
package sample

import org.specs2.Specification

class AddressMapperSpec extends Specification with Goose {
  def is =  pending /*"Street mapping" ^`street mapping` ^
            "Address mapping" ^`address mapping` ^
            end*/
    
  def `address mapping` = check(new DatabaseBackedAddressMapper(_:Database).mapAddress(_:String)) {(database, addressId) => 
    _.when(addressId ==> "123").
      and(database.stub(_.find("addresses", "123")) ==> Some(Map("city" -> "789", "street" -> "999"))).
      and(database.stub(_.find("cities", "789")) ==> Some(Map("name" -> "Curitiba"))).
      and(database.stub(_.find("streets", "999")) ==> Some(Map("name" -> "St. st."))).
      then(_ must beSome(Address(City("Curitiba"), Street("St. st.")))).but {
        _.when(database.stub(_.find("streets", "999")) ==>  None).
          then(_  must beNone)
      }.
      but {
        _.when(database.stub(_.find("cities", "789")) ==> None).
          then(_ must beNone)
      }
  }
  
  def `street mapping` = check(new DatabaseBackedAddressMapper(_:Database).mapStreet(_:String)) {(database, streetId) =>
    _.when(streetId ==> "951").
    and(database.stub(_.find("streets", "951")) ==> Some(Map("name" -> "St. st."))).
      then(_ must beSome(Street("St. st."))).but {
        _.when(database.stub(_.find("streets", "951")) ==>  None).
          then(_ must_== None)
      }
  }
}