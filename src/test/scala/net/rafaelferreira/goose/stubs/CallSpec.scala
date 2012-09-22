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
  
  def capture[T](methodCall: T => Any): Call[T] = macro CallMacro.capture_impl[T]
  
  def is = "Capturing" ^
    "no-arguments method" ! noArguments ^
    "method that takes one integer argument" ! argumentTakingInt ^
    "method that takes two arguments" ! takingTwoArguments ^
    "method whose closure takes an explicit parameter list" ! explicitParams ^
    "method whose closure takes an explicit typed parameter list" ! explicitTypedParams
  
  def noArguments = {
    val call = capture[Dummy](_.noArgs)
    call.method must_== "noArgs"
  }
  
  def argumentTakingInt = {
    val call = capture[Dummy](_.oneIntArg(42))
    call must_== Call(this, "oneIntArg", Seq(42))
  }
  
  
  def takingTwoArguments = {
    val call = capture[Dummy](_.twoArgs(42, "text"))
    call must_== Call(this, "twoArgs", Seq(42, "text")) 
  }
  
  
  def explicitParams = {
    val call = capture[Dummy]((x) => x.oneIntArg(42))
    call must_== Call(this, "oneIntArg", Seq(42))
  }
  
  def explicitTypedParams = {
    val call = capture((x:Dummy) => x.oneIntArg(42))
    call must_== Call(this, "oneIntArg", Seq(42))
  }

}
