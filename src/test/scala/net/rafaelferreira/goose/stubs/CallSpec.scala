package net.rafaelferreira.goose
package stubs

import org.specs2.Specification

import scala.language.experimental.macros

class CallSpec extends Specification {
  trait Dummy {
    def noArgs:String
    def oneIntArg(i:Int)
    def twoArgs(i:Int,s:String)
  }
  
  class Enclosing {
    def capture[T](methodCall: T => Any): Call[T] = macro CallMacro.capture_impl[T]
  }
  
  val enclosing = new Enclosing
  
  def is = "Capturing" ^
    "no-arguments method" ! noArguments ^
    "method that takes one integer argument" ! argumentTakingInt ^
    "method that takes two arguments" ! takingTwoArguments ^
    "method whose closure takes an explicit parameter list" ! explicitParams ^
    "method whose closure takes an explicit typed parameter list" ! explicitTypedParams ^
    "method indirectly" ! indirectly
  
  def noArguments = {
    val call = enclosing.capture[Dummy](_.noArgs)
    call must_== Call(enclosing, "noArgs", Nil)
  }

  def argumentTakingInt = {
    val call = enclosing.capture[Dummy](_.oneIntArg(42))
    call must_== Call(enclosing, "oneIntArg", Seq(42))
  }
  
  
  def takingTwoArguments = {
    val call = enclosing.capture[Dummy](_.twoArgs(42, "text"))
    call must_== Call(enclosing, "twoArgs", Seq(42, "text")) 
  }
  
  
  def explicitParams = {
    val call = enclosing.capture[Dummy]((x) => x.oneIntArg(42))
    call must_== Call(enclosing, "oneIntArg", Seq(42))
  }
  
  def explicitTypedParams = {
    val call = enclosing.capture((x:Dummy) => x.oneIntArg(42))
    call must_== Call(enclosing, "oneIntArg", Seq(42))
  }

  def indirectly = {
    val reference = enclosing
    val call = reference.capture[Dummy](_.noArgs)
    call must_== Call(enclosing, "noArgs", Nil)
  }

}
  
