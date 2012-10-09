package net.rafaelferreira.goose
import scala.reflect.ClassTag

import org.specs2.specification.Fragments
import org.specs2.Specification


trait CheckingForVariousArities extends CheckHelpers {self: GooseSpecificationDSL with Specification =>
  def check[T1: ClassTag, R](resultExpression: (T1) => R)(testDefinition: (Dependency[T1]) => When[R] => When[R]): Fragments = {
    val (dep1) = (newDependency[T1]("1"))
    val calcResult = {state:Environment =>
      (state.valueFor(dep1)) match {
        case (Some(value1)) => Right(resultExpression(value1))
        case (double1) => Left(reportMissing(Seq(double1)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, R](resultExpression: (T1, T2) => R)(testDefinition: (Dependency[T1], Dependency[T2]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2) = (newDependency[T1]("1"), newDependency[T2]("2"))
    val calcResult = {state:Environment =>
      (state.valueFor(dep1), state.valueFor(dep2)) match {
        case (Some(value1), Some(value2)) => Right(resultExpression(value1, value2))
        case (double1, double2) => Left(reportMissing(Seq(double1, double2)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, R](resultExpression: (T1, T2, T3) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"))
    val calcResult = {state:Environment =>
      (state.valueFor(dep1), state.valueFor(dep2), state.valueFor(dep3)) match {
        case (Some(value1), Some(value2), Some(value3)) => Right(resultExpression(value1, value2, value3))
        case (double1, double2, double3) => Left(reportMissing(Seq(double1, double2, double3)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3)(when).results
  }

 }
