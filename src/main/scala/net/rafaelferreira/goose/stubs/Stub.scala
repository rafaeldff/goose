package net.rafaelferreira.goose
package stubs

import java.lang.reflect.Method
import org.specs2.matcher.Matcher

import scala.language.experimental.macros

case class Stub[T: ClassManifest](expectations: Seq[Expectation[T]] = Vector()) {
  lazy val results = 
    expectations.foldLeft(Map[Method, AnyRef]()) {(map, expectation) =>
      map + (expectation.methodCalled -> expectation.result) 
    }
  
  def expecting[R](expectation:Expectation[T]):Stub[T] = copy(expectations = expectations :+ expectation) 
  
  def stubObject: T = 
    ProxyFactory { (obj:Object, method:Method, args:Array[Object]) =>
      results(method)
    }
}

case class Expectation[T:ClassManifest](call: T => Any, result: AnyRef) {
  val methodCalled = {
    val recorder = new Recorder[T]
    call(recorder())
    recorder.methodCalled
  }
  
}

case class Call(method:String, args:Seq[Any])

object Call {
  def capture[T,R](methodCall: T => R): Call = macro CallMacro.capture_impl[T,R]
}

class Recorder[T:ClassManifest] {
  /*
   * This class encapsulates the mutability inherent in working with proxies 
   */
  
  val javaClass = implicitly[ClassManifest[T]].erasure
  
  var call: Option[Method] = None
  
  private val dummy = ProxyFactory { (obj:Object, method:Method, args:Array[Object]) =>
    call = Some(method);
    null
  }
  
  def apply(): T = dummy
  def methodCalled:Method = call.getOrElse { 
    throw new IllegalStateException(
      "Expecting a call on proxy object of class '%s' but none was made." format javaClass.getName
    ) 
  } 
}

object ProxyFactory {
  import java.lang.reflect.{Array=>_, _}
  
  type ProxyReaction = (AnyRef, Method, Array[AnyRef]) => AnyRef
  
  def apply[T:ClassManifest](reaction: ProxyReaction):T = { 
      val javaClass = implicitly[ClassManifest[T]].erasure
      ProxyFactory.make[T](javaClass)(reaction)
  }
  
  def make[Result](cls:Class[_]*)(reaction:ProxyReaction):Result = {
    val handler = new InvocationHandler {
    def invoke(obj:Object, method:Method, args:Array[Object]) = 
        reaction(obj, method, args)
    }
        
    Proxy.newProxyInstance(Thread.currentThread.getContextClassLoader, Array(cls:_*), handler).asInstanceOf[Result]
  } 
}

trait FakeArgument {
  def $gooseMatcherMethod: Matcher[_]
}
  
trait StubsStructure {
  
  implicit def argThat[T: ClassManifest, U <: T](m: Matcher[U]): T = {
    val parameterClass = implicitly[ClassManifest[T]].erasure
    ProxyFactory.make(parameterClass, classOf[FakeArgument]) {(obj:AnyRef, method:Method, args:Array[AnyRef]) =>
      method.getName match {
        case "$gooseMatcherMethod" => m
        case _ => null
      }
    }
  }
    
}