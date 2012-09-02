package net.rafaelferreira.goose

import org.specs2.specification.Fragments
import org.specs2.Specification


trait CheckingForVariousArities extends CheckHelpers {self: GooseStructure with Specification =>
  def check[T1: ClassManifest, R](resultExpression: (T1) => R)(testDefinition: (Dependency[T1]) => When[R] => When[R]): Fragments = {
    val (dep1) = (dep[T1])
    val calcResult = {state:State =>
      val (value1) = (state.get(dep1))
      
      whenAllPresent(Seq(value1)) { resultExpression(value1.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1)(when).results
  }

  def check[T1: ClassManifest, T2: ClassManifest, R](resultExpression: (T1, T2) => R)(testDefinition: (Dependency[T1], Dependency[T2]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2) = (dep[T1], dep[T2])
    val calcResult = {state:State =>
      val (value1, value2) = (state.get(dep1), state.get(dep2))
      
      whenAllPresent(Seq(value1, value2)) { resultExpression(value1.get, value2.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2)(when).results
  }

  def check[T1: ClassManifest, T2: ClassManifest, T3: ClassManifest, R](resultExpression: (T1, T2, T3) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3) = (dep[T1], dep[T2], dep[T3])
    val calcResult = {state:State =>
      val (value1, value2, value3) = (state.get(dep1), state.get(dep2), state.get(dep3))
      
      whenAllPresent(Seq(value1, value2, value3)) { resultExpression(value1.get, value2.get, value3.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3)(when).results
  }
}