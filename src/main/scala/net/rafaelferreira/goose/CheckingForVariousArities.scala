package net.rafaelferreira.goose
import scala.reflect.ClassTag

import org.specs2.specification.Fragments
import org.specs2.Specification


trait CheckingForVariousArities extends CheckHelpers {self: GooseStructure with Specification =>
  def check[T1: ClassTag, R](resultExpression: (T1) => R)(testDefinition: (Dependency[T1]) => When[R] => When[R]): Fragments = {
    val (dep1) = (dep[T1]("1"))
    val calcResult = {state:State =>
      (state.get(dep1)) match {
        case (InitializedDouble(value1)) => Right(resultExpression(value1))
        case (double1) => Left(reportMissing(Seq(double1)))
      }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, R](resultExpression: (T1, T2) => R)(testDefinition: (Dependency[T1], Dependency[T2]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2) = (dep[T1]("1"), dep[T2]("2"))
    val calcResult = {state:State =>
      (state.get(dep1), state.get(dep2)) match {
        case (InitializedDouble(value1), InitializedDouble(value2)) => Right(resultExpression(value1, value2))
        case (double1, double2) => Left(reportMissing(Seq(double1, double2)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, R](resultExpression: (T1, T2, T3) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3]) => When[R] => When[R]): Fragments ={
    val (dep1, dep2, dep3) = (dep[T1]("1"), dep[T2]("2"), dep[T3]("3"))
    val calcResult = {state:State =>
      (state.get(dep1), state.get(dep2), state.get(dep3)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3)) => Right(resultExpression(value1, value2, value3))
        case (double1, double2, double3) => Left(reportMissing(Seq(double1, double2, double3)))
      }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3)(when).results
  }
}
