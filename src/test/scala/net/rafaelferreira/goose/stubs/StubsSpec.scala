package net.rafaelferreira.goose
package stubs

import org.specs2.Specification

class StubsSpec extends Specification {
  trait Bar {}
  trait Foo {
    def foo:String
    def takesBar(bar:Bar):String
  }
  
  def is =  "Stub" ^ p ^
              "Giving argument matchers" ^ 
                "stub a no-args method" ! stubNoArgs ^
                "stub a method that takes an argument typed to a trait" ! stubWithArgs ^
                "stub a method that may take different arguments" ! stubWithDifferentArgs ^ 
                "stub a method twice, with the second expectation overriding the first " ^ stubOverriding ^ p ^
              "Giving literal arguments" ^
                "stub a method that may take different arguments" ! stubWithDifferentLiteralArgs ^ p ^
              "Giving mixed arguments" ^
                "stub a method that may take different arguments" ! stubWithDifferentMixedArgs

  
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
    val differentBarParameter = new Bar {}
    val expectingFirst = stub.expecting(Expectation(Call(this, "takesBar", Seq(===(barParameter))), "first"))
    val expectingBoth = expectingFirst.expecting(Expectation(Call(this, "takesBar", Seq(===(differentBarParameter))), "second"))
    (expectingBoth.value.takesBar(barParameter) must_== "first") and (expectingBoth.value.takesBar(differentBarParameter) must_== "second")
  }
  
  def stubOverriding = {
    val stub = new StubDouble[Foo]
    val barParameter = new Bar {}
    val expectingFirst = stub.expecting(Expectation(Call(this, "takesBar", Seq(===(barParameter))), "first"))
    val expectingSecond = expectingFirst.expecting(Expectation(Call(this, "takesBar", Seq(===(barParameter))), "second"))
    expectingSecond.value.takesBar(barParameter) must_== "second"
  }
  
  def stubWithDifferentLiteralArgs = {
    val stub = new StubDouble[Foo]
    val barParameter = new Bar {}
    val differentBarParameter = new Bar {}
    val expectingFirst = stub.expecting(Expectation(Call(this, "takesBar", Seq(barParameter)), "first"))
    val expectingBoth = expectingFirst.expecting(Expectation(Call(this, "takesBar", Seq(differentBarParameter)), "second"))
    
    (expectingBoth.value.takesBar(barParameter) must_== "first") and (expectingBoth.value.takesBar(differentBarParameter) must_== "second")
  }
  
  def stubWithDifferentMixedArgs = {
    val stub = new StubDouble[Foo]
    val barParameter = new Bar {}
    val differentBarParameter = new Bar {}
    val yetAnotherBarParameter = new Bar {}
    
    val expectingFirst = stub.expecting(Expectation(Call(this, "takesBar", Seq(barParameter)), "first"))
    val expectingSecond = expectingFirst.expecting(Expectation(Call(this, "takesBar", Seq(===(differentBarParameter))), "second"))
    val expectingAllThree = expectingSecond.expecting(Expectation(Call(this, "takesBar", Seq(yetAnotherBarParameter)), "third"))
    
    (expectingAllThree.value.takesBar(barParameter) must_== "first") and 
    (expectingAllThree.value.takesBar(differentBarParameter) must_== "second") and
    (expectingAllThree.value.takesBar(yetAnotherBarParameter) must_== "third")
  }
  
}
