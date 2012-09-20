package net.rafaelferreira.goose
package stubs

import org.specs2.Specification

class StubsSpec extends Specification with StubsStructure {
  trait Bar {}
  trait Foo {
    def foo:String
    def takesBar(bar:Bar):String
  }
  
  def is =  "Stub" ^ p ^
              "stub a no-args method" ! stubNoArgs ^
              "stub a method that takes an argument typed to a trait" ! stubWithArgs ^
              "stub a method that may take different arguments" ! stubWithDifferentArgs
  
  def stubNoArgs = {
    val stub = new Stub[Foo]
    val expecting = stub.expecting(Expectation(Call(this, "foo", Nil), "result"))
    expecting.stubObject.foo must_== "result"
  }
  
  def stubWithArgs = {
    val stub = new Stub[Foo]
    val barParameter = new Bar {}
    val expecting = stub.expecting(Expectation(Call(this, "takesBar", Seq(===(barParameter))), "result"))
    expecting.stubObject.takesBar(barParameter) must_== "result"
  }
  
  def stubWithDifferentArgs = {
    val stub = new Stub[Foo]
    val barParameter = new Bar {}
    val expectingFirst = stub.expecting(Expectation(Call(this, "takesBar", Seq(===(barParameter))), "result"))
    expectingFirst.stubObject.takesBar(barParameter) must_== "result"
  }
  
}
