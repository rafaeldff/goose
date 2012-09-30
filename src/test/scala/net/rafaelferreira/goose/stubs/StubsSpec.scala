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
    val stub = new StubDouble[Foo]
    val expecting = stub.expecting(Expectation(Call(this, "foo", Nil), "result"))
    expecting.value.foo must_== "result"
  }
  
  def stubWithArgs = {
    val stub = new StubDouble[Foo]
    val barParameter = new Bar {}
    val expecting = stub.expecting(Expectation(Call(this, "takesBar", Seq(===(barParameter))), "result"))
    expecting.value.takesBar(barParameter) must_== "result"
  }
  
  def stubWithDifferentArgs = {
    val stub = new StubDouble[Foo]
    val barParameter = new Bar {}
    val expectingFirst = stub.expecting(Expectation(Call(this, "takesBar", Seq(===(barParameter))), "result"))
    expectingFirst.value.takesBar(barParameter) must_== "result"
  }
  
}
