package net.rafaelferreira.goose

import org.specs2.specification.Fragments
import org.specs2.Specification

trait CheckingForVariousArities {self: GooseStructure with Specification => 
  def check[T1: ClassManifest, R](resultExpression: (T1) => R)(c: (Dependency[T1]) => When[R] => When[R]): Fragments = {
    val dep1 = dep[T1]
    val when = new When[R]()((state: State) => resultExpression(state.get(dep1).get))
    c(dep1)(when).results ^ end
  }

  def check[T1: ClassManifest, T2: ClassManifest, R](resultExpression: (T1, T2) => R)(c: (Dependency[T1], Dependency[T2]) => When[R] => When[R]): Fragments = {
    val dep1 = dep[T1]
    val dep2 = dep[T2]
    val when = new When[R]()((state: State) => resultExpression(state.get(dep1).get, state.get(dep2).get))
    c(dep1, dep2)(when).results ^ end
  }

  def check[T1: ClassManifest, T2: ClassManifest, T3: ClassManifest, R](resultExpression: (T1, T2, T3) => R)(c: (Dependency[T1], Dependency[T2], Dependency[T3]) => When[R] => When[R]): Fragments = {
    val dep1 = dep[T1]
    val dep2 = dep[T2]
    val dep3 = dep[T3]
    val when = new When[R]()((state: State) => resultExpression(state.get(dep1).get, state.get(dep2).get, state.get(dep3).get))
    c(dep1, dep2, dep3)(when).results ^ end
  }
}