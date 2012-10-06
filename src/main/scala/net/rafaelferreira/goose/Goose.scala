package net.rafaelferreira
package goose

import scala.reflect.ClassTag

trait Goose extends GooseSpecificationDSL with CheckingForVariousArities with stubs.StubsDSL with direct.DirectDSL {
  class ActualDependency[T: ClassTag](name:String) extends GeneralDependency[T] with DirectDependency[T] with StubDependency[T] {self =>
    override def toString = "DEP[%s]" format name
  }
  
  type Dependency[T] = ActualDependency[T]
  
  def newDependency[T: ClassTag](name:String): ActualDependency[T] = new ActualDependency[T](name)
}