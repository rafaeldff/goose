package net.rafaelferreira.goose
package sample

import org.specs2.Specification

class AddressMapperSpec extends Specification with Goose {
  def is =  "Street mapping" ^ `street mapping` ^
            "Address mapping" ^ `address mapping` ^
            end
    
  def `address mapping` = check(new DatabaseBackedAddressMapper(_:Database).mapAddress(_:String)) {(database, addressId) => 
    _.when(addressId ==> "123").
      and(database(_.find("addresses", addressId)) ==> Some(Map("city" -> "789", "street" -> "999"))).
      and(database(_.find("cities", "789")) ==> Some(Map("name" -> "Curitiba"))).
      and(database(_.find("streets", "999")) ==> Some(Map("name" -> "St. st."))).
      then(_ must beSome(Address(City("Curitiba"), Street("St. st.")))).
      but {
        _.when(database(_.find("streets", "999")) ==>  None).
          then(_  must beNone)
      }.
      but {
        _.when(database(_.find("cities", "789")) ==> None).
          then(_ must beNone)
      }
  }
  
  def `street mapping` = check(new DatabaseBackedAddressMapper(_:Database).mapStreet(_:String)) {(database, streetId) =>
    _.when(streetId ==> "951").
     and(database(_.find("streets", "951")) ==> Some(Map("name" -> "St. st."))).
     then(_ must beSome(Street("St. st."))).but {
       _.when(database(_.find("streets", "951")) ==>  None).
         then(_ must_== None)
     }
  }
}