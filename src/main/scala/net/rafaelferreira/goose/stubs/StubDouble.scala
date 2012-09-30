package net.rafaelferreira.goose
package stubs

import java.lang.reflect.Method
import org.specs2.matcher.Matcher
import scala.language.experimental.macros
import scala.reflect.ClassTag

case class StubDouble[T: ClassTag](expectations: Seq[Expectation[T]] = Vector()) extends InitializedDouble[T] {
  lazy val results = 
    expectations.foldLeft(Map[String, AnyRef]()) {(map, expectation) =>
      map + (expectation.methodCalled -> expectation.result) 
    }
  
  def expecting[R](expectation:Expectation[T]):StubDouble[T] = 
    copy(expectations = expectations :+ expectation) 
  
  def value: T = 
    ProxyFactory { (obj:Object, method:Method, args:Array[Object]) =>
      results(method.getName)
    }
}

case class Expectation[T:ClassTag](call: Call[T], result: AnyRef) {
  val methodCalled = call.method
  
}

object ProxyFactory {
  import java.lang.reflect.{Array=>_, _}
  
  type ProxyReaction = (AnyRef, Method, Array[AnyRef]) => AnyRef
  
  def apply[T:ClassTag](reaction: ProxyReaction):T = { 
      val javaClass = implicitly[ClassTag[T]].runtimeClass
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
  
  /*implicit def argThat[T: ClassTag, U <: T](m: Matcher[U]): T = {
    val parameterClass = implicitly[ClassTag[T]].runtimeClass
    ProxyFactory.make(parameterClass, classOf[FakeArgument]) {(obj:AnyRef, method:Method, args:Array[AnyRef]) =>
      method.getName match {
        case "$gooseMatcherMethod" => m
        case _ => null
      }
    }
  }*/
    
}
