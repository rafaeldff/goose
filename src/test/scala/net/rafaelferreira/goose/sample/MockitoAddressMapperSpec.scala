package net.rafaelferreira.goose
package sample

import org.specs2.Specification
import org.specs2.mock.Mockito

class MockitoAddressMapperSpec extends Specification with Mockito {
  def is =  "Address mapping" ^
      "should be able to map an address" ! `be able to map` ^ 
      "should return None if no city can be found" ! `return none for no city` ^
      "should return None if no street can be found" ! `return none for no street` ^
      end
  
  def `be able to map` = {
    val database = mock[Database]
    
    val addressId = "123"
      
    database.find("addresses", addressId) returns Some(Map("city" -> "789", "street" -> "999"))
    
    database.find("cities", "789") returns Some(Map("name" -> "Curitiba"))
    
    database.find("streets", "999") returns Some(Map("name" -> "St. st."))
    
    val mapper = new DatabaseBackedAddressMapper(database)
    
    mapper.mapAddress(addressId) must beSome(Address(City("Curitiba"), Street("St. st.")))
  }
  
  def `return none for no city` = {
    val database = mock[Database]
    
    val addressId = "123"
    
    database.find("addresses", addressId) returns Some(Map("city" -> "789", "street" -> "999"))
    
    database.find("cities", "789") returns None
    
    database.find("streets", "999") returns Some(Map("name" -> "St. st."))
    
    val mapper = new DatabaseBackedAddressMapper(database)
    
    mapper.mapAddress(addressId) must beNone
  }
  
  def `return none for no street` = {
    val database = mock[Database]
    
    val addressId = "123"
    
    database.find("addresses", addressId) returns Some(Map("city" -> "789", "street" -> "999"))
    
    database.find("cities", "789") returns Some(Map("name" -> "Curitiba"))
    
    database.find("streets", "999") returns None
    
    val mapper = new DatabaseBackedAddressMapper(database)
    
    mapper.mapAddress(addressId) must beNone
  }
  
}
