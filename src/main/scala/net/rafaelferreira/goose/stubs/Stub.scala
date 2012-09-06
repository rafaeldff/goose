package net.rafaelferreira.goose
package stubs

import java.lang.reflect.{Method, InvocationHandler}

case class Stub[T: ClassManifest](expectations: Seq[Expectation[T]] = Vector()) {
  lazy val results = 
    expectations.foldLeft(Map[String,AnyRef]()) {(map, expectation) =>
      map + (expectation.methodCalled -> expectation.result) 
    }
  
  def expecting[R](expectation:Expectation[T]):Stub[T] = copy(expectations = expectations :+ expectation) 
  
  def stubObject: T = ProxyFactory { (obj:Object, method:Method, args:Array[Object]) =>
    results(method.getName)
  }
}

case class Expectation[T:ClassManifest](call: T => Any, result: AnyRef) {
  val methodCalled = {
    val recorder = new Recorder[T]
    call(recorder())
    recorder.methodCalled
  }
}

class Recorder[T:ClassManifest] {
  /*
   * This class encapsulates the mutability inherent in working with proxies 
   */
  
  import java.lang.reflect.{Array=>ReflArray, _}
  val javaClass = implicitly[ClassManifest[T]].erasure
  
  var call: Option[String] = None
  
  private val dummy = ProxyFactory { (obj:Object, method:Method, args:Array[Object]) =>
    call = Some(method.getName);
    null
  }
  
  def apply(): T = dummy
  def methodCalled:String = call.getOrElse { 
    throw new IllegalStateException(
      "Expecting a call on proxy object of class '%s' but none was made." format javaClass.getName
    ) 
  } 
}

object ProxyFactory {
  import java.lang.reflect.{Array=>_, _}
  
  type ProxyReaction = (Object, Method, Array[Object]) => AnyRef
  
  def apply[T:ClassManifest](reaction: ProxyReaction) = { 
    val handler = new InvocationHandler {
      def invoke(obj:Object, method:Method, args:Array[Object]) = 
        reaction(obj, method, args)
    }
    
    val javaClass = implicitly[ClassManifest[T]].erasure
    Proxy.newProxyInstance(Thread.currentThread.getContextClassLoader, Array(javaClass), handler).asInstanceOf[T]
  }
}
