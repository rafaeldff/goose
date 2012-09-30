package net.rafaelferreira.goose
package stubs

import scala.reflect.ClassTag

object ProxyFactory {
  import java.lang.reflect.{Array=>_, Method => ReflectMethod, _}
  
  type Subject = AnyRef
  type Arguments = Array[AnyRef]
  type Method = ReflectMethod
  
  type ProxyReaction = (Subject, Method, Arguments) => AnyRef
  
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
