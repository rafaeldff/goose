package net.rafaelferreira.goose

import org.specs2.specification.Fragments
import org.specs2.Specification


trait CheckingForVariousArities {self: GooseStructure with Specification =>
  private def missingValues(vs: Seq[Option[Any]]) = vs.zipWithIndex.collect {case (None, i) => i} 
  
  def check[T1: ClassManifest, R](resultExpression: (T1) => R)(testDefinition: (Dependency[T1]) => When[R] => When[R]): Fragments = {
    val (dep1) = (dep[T1])
    val calcResult = {state:State =>
      val (value1) = (state.get(dep1))
      
      missingValues(Seq(value1)) match {
        case Nil => Right(resultExpression(value1.get))
        case _ => Left("No value was supplied for dependencies %s. Did you forget 'when' or 'and' clauses?" format (missingValues(Seq(value1))).mkString("[", ",", "]"))  
      }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1)(when).results
  }

  def check[T1: ClassManifest, T2: ClassManifest, R](resultExpression: (T1, T2) => R)(testDefinition: (Dependency[T1], Dependency[T2]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2) = (dep[T1], dep[T2])
    val calcResult = {state:State =>
      val (value1, value2) = (state.get(dep1), state.get(dep2))
      
      missingValues(Seq(value1, value2)) match {
        case Nil => Right(resultExpression(value1.get, value2.get))
        case _ => Left("No value was supplied for dependencies %s. Did you forget 'when' or 'and' clauses?" format (missingValues(Seq(value1, value2))).mkString("[", ",", "]"))  
      }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2)(when).results
  }

  def check[T1: ClassManifest, T2: ClassManifest, T3: ClassManifest, R](resultExpression: (T1, T2, T3) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3) = (dep[T1], dep[T2], dep[T3])
    val calcResult = {state:State =>
      val (value1, value2, value3) = (state.get(dep1), state.get(dep2), state.get(dep3))
      
      missingValues(Seq(value1, value2, value3)) match {
        case Nil => Right(resultExpression(value1.get, value2.get, value3.get))
        case _ => Left("No value was supplied for dependencies %s. Did you forget 'when' or 'and' clauses?" format (missingValues(Seq(value1, value2, value3))).mkString("[", ",", "]"))  
      }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3)(when).results
  }
}