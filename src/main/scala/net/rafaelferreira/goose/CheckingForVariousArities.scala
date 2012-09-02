package net.rafaelferreira.goose

import org.specs2.specification.Fragments
import org.specs2.Specification

trait CheckingForVariousArities {self: GooseStructure with Specification => 
  def check[T1: ClassManifest, R](resultExpression: (T1) => R)(c: (Dependency[T1]) => When[R] => When[R]): Fragments = {
    val dep1 = dep[T1]
    val when = new When[R]()((state: State) => {
      state.get(dep1) match {
        case Some(value1) => Right(resultExpression(value1))
        case None => Left("No value was supplied for the 1st dependency. Did you forget a 'when' or 'and' clause?")
      }
    })
    c(dep1)(when).results ^ end
  }

  def check[T1: ClassManifest, T2: ClassManifest, R](resultExpression: (T1, T2) => R)(c: (Dependency[T1], Dependency[T2]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2) = (dep[T1], dep[T2])
    val calcResult = (state:State) => state.get(dep1) match {
      case None => Left("No value was supplied for the 1st dependency. Did you forget a 'when' or 'and' clause?")
      case Some(value1) =>
        state.get(dep2) match {
          case None => Left("No value was supplied for the 2st dependency. Did you forget a 'when' or 'and' clause?")
          case Some(value2) => Right(resultExpression(value1, value2))
        }
    }
    
    val when = new When[R]()(calcResult)
    c(dep1, dep2)(when).results ^ end
  }

  def check[T1: ClassManifest, T2: ClassManifest, T3: ClassManifest, R](resultExpression: (T1, T2, T3) => R)(c: (Dependency[T1], Dependency[T2], Dependency[T3]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3) = (dep[T1], dep[T2], dep[T3]) 
    val calcResult = (state:State) => state.get(dep1) match {
      case None => Left("No value was supplied for the 1st dependency. Did you forget a 'when' or 'and' clause?")
      case Some(value1) =>
        state.get(dep2) match {
          case None => Left("No value was supplied for the 2st dependency. Did you forget a 'when' or 'and' clause?")
          case Some(value2) => state.get(dep3) match {
            case None => Left("No value was supplied for the 3rd dependency. Did you forget a 'when' or 'and' clause?")
            case Some(value3) => Right(resultExpression(value1, value2, value3))
          }
        }
    }
      
    val when = new When[R]()(calcResult)
    c(dep1, dep2, dep3)(when).results ^ end
  }
}