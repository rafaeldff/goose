package net.rafaelferreira.goose
package sample

import org.specs2.Specification
import org.specs2.mock.Mockito

class BetterMockitoAddressMapperSpec extends Specification with Mockito {
  def is = "Address mapping" ^
    "should be able to map an address" ! addressSpec().`be able to map` ^
    "should return None if no city can be found" ! addressSpec().`return none for no city` ^
    "should return None if no street can be found" ! addressSpec().`return none for no street` ^
    end

  case class addressSpec {
    val database = mock[Database]

    val addressId = "123"

    database.find("addresses", addressId) returns Some(Map("city" -> "789", "street" -> "999"))

    database.find("cities", "789") returns Some(Map("name" -> "Curitiba"))

    database.find("streets", "999") returns Some(Map("name" -> "St. st."))

    val mapper = new DatabaseBackedAddressMapper(database)

    def `be able to map` = {
      mapper.mapAddress(addressId) must beSome(Address(City("Curitiba"), Street("St. st.")))
    }

    def `return none for no city` = {
      database.find("cities", "789") returns None

      mapper.mapAddress(addressId) must beNone
    }

    def `return none for no street` = {
      database.find("streets", "999") returns None

      mapper.mapAddress(addressId) must beNone
    }
  }
}
