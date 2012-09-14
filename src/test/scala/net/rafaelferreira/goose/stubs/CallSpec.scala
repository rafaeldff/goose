package net.rafaelferreira.goose
package stubs

import org.specs2.Specification

class CallSpec extends Specification {
  trait Dummy {
    def noArgs:String
  }
  def is = {
   val call = Call.capture[Dummy](_.noArgs)
   call.method must_== "noArgs"
  }
}
