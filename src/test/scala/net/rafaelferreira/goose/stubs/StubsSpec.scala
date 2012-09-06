package net.rafaelferreira.goose
package stubs

import org.specs2.Specification

class StubsSpec extends Specification {
  trait Foo {
    def foo:String  
  }
  
  def is =  "Stub" ^ p ^
              "stub no-args method" ! stubNoArgs ^
            "Recorder" ^ p ^
              "call recorder records last call" ! recorderOk ^
              "call recorder raises exception if we try to fetch a call when none was made" ! recorderFailure
  
  def stubNoArgs = {
    val stub = new Stub[Foo]
    val expecting = stub.expecting(Expectation({_.foo}, "result"))
    expecting.stubObject.foo must_== "result"
  }
  
  def recorderOk = {
    val recorder = new Recorder[Foo]
    recorder().foo
    recorder.methodCalled should_== "foo"
  }
  
  def recorderFailure = {
    val recorder = new Recorder[Foo]
    recorder.methodCalled should throwA[IllegalStateException].like { 
      case e => e.getMessage must beMatching("Expecting a call on proxy object of class '.*Foo' but none was made.".r)  
    } 
      
  }
  
}