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
              "stub a method that may take different arguments" ! stubWithDifferentArgs ^
            "Recorder" ^ p ^
              "call recorder records last call" ! recorderOk ^
              "call recorder raises exception if we try to fetch a call when none was made" ! recorderFailure
  
  def stubNoArgs = {
    val stub = new Stub[Foo]
    val expecting = stub.expecting(Expectation({_.foo}, "result"))
    expecting.stubObject.foo must_== "result"
  }
  
  def stubWithArgs = {
    val stub = new Stub[Foo]
    val barParameter = new Bar {}
    val expecting = stub.expecting(Expectation({_.takesBar(===(barParameter))}, "result"))
    expecting.stubObject.takesBar(barParameter) must_== "result"
  }
  
  def stubWithDifferentArgs = {
    val stub = new Stub[Foo]
    val barParameter = new Bar {}
    val expectingFirst = stub.expecting(Expectation({_.takesBar(===(barParameter))}, "result"))
    expectingFirst.stubObject.takesBar(barParameter) must_== "result"
  }
  
  def recorderOk = {
    val recorder = new Recorder[Foo]
    recorder().foo
    recorder.methodCalled.getName should_== "foo"
  }
  
  def recorderFailure = {
    val recorder = new Recorder[Foo]
    recorder.methodCalled should throwA[IllegalStateException].like { 
      case e => e.getMessage must beMatching("Expecting a call on proxy object of class '.*Foo' but none was made.".r)  
    } 
      
  }
  
}
