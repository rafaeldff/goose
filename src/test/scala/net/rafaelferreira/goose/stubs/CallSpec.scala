package net.rafaelferreira.goose
package stubs

import org.specs2.Specification
import scala.language.experimental.macros

class CallSpec extends Specification {
  trait Foo {}
  
  trait Dummy {
    def noArgs:String
    def oneIntArg(i:Int)
    def twoArgs(i:Int,s:String)
    def takesFoo(foo:Foo)
    def mixed(firstFoo:Foo,i:Int,secondFoo:Foo)
  }
  
  class Enclosing {
    def capture[T](methodCall: T => Any): Call[T] = macro CallMacro.capture_impl[T]
  }
  
  val enclosing = new Enclosing
  
  def is = args.report(failtrace=true) ^"Capturing" ^
    "no-arguments method" ! noArguments ^
    "method that takes one integer argument" ! argumentTakingInt ^
    "method that takes two arguments" ! takingTwoArguments ^
    "method whose closure takes an explicit parameter list" ! explicitParams ^
    "method whose closure takes an explicit typed parameter list" ! explicitTypedParams ^
    "method indirectly" ! indirectly ^
    "an argument implicitly convert from a Dependency will capture the dependency" ! capturingDependency ^
    "integer, object and implicit-dependency arguments" ! mixedArguments ^
    end 
  
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
  
  def capturingDependency = {
    val fooDependency = new GeneralDependency[Foo] {}
    val call = enclosing.capture((_:Dummy).takesFoo(fooDependency))
    call.args must_== Seq(fooDependency)
  }
  
  def mixedArguments = {
    val foo = new Foo {}
    val integer = 42
    val fooDependency = new GeneralDependency[Foo] {}
    val call = enclosing.capture((_: Dummy).mixed(foo, integer, fooDependency))
    call must_== Call(enclosing, "mixed", Seq(foo, integer, fooDependency))
  }
}
