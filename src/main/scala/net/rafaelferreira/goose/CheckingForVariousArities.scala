package net.rafaelferreira.goose
import scala.reflect.ClassTag

import org.specs2.specification.Fragments
import org.specs2.Specification


trait CheckingForVariousArities extends CheckHelpers {self: GooseStructure with Specification =>
  def check[T1: ClassTag, R](resultExpression: (T1) => R)(testDefinition: (Dependency[T1]) => When[R] => When[R]): Fragments = {
    val (dep1) = (newDependency[T1]("1"))
    val calcResult = {state:Environment =>
      (state.get(dep1)) match {
        case (InitializedDouble(value1)) => Right(resultExpression(value1))
        case (double1) => Left(reportMissing(Seq(double1)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, R](resultExpression: (T1, T2) => R)(testDefinition: (Dependency[T1], Dependency[T2]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2) = (newDependency[T1]("1"), newDependency[T2]("2"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2)) match {
        case (InitializedDouble(value1), InitializedDouble(value2)) => Right(resultExpression(value1, value2))
        case (double1, double2) => Left(reportMissing(Seq(double1, double2)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, R](resultExpression: (T1, T2, T3) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3)) => Right(resultExpression(value1, value2, value3))
        case (double1, double2, double3) => Left(reportMissing(Seq(double1, double2, double3)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, R](resultExpression: (T1, T2, T3, T4) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4)) => Right(resultExpression(value1, value2, value3, value4))
        case (double1, double2, double3, double4) => Left(reportMissing(Seq(double1, double2, double3, double4)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5)) => Right(resultExpression(value1, value2, value3, value4, value5))
        case (double1, double2, double3, double4, double5) => Left(reportMissing(Seq(double1, double2, double3, double4, double5)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6)) => Right(resultExpression(value1, value2, value3, value4, value5, value6))
        case (double1, double2, double3, double4, double5, double6) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7))
        case (double1, double2, double3, double4, double5, double6, double7) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8))
        case (double1, double2, double3, double4, double5, double6, double7, double8) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"), newDependency[T11]("11"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10), InitializedDouble(value11)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"), newDependency[T11]("11"), newDependency[T12]("12"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10), InitializedDouble(value11), InitializedDouble(value12)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"), newDependency[T11]("11"), newDependency[T12]("12"), newDependency[T13]("13"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10), InitializedDouble(value11), InitializedDouble(value12), InitializedDouble(value13)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"), newDependency[T11]("11"), newDependency[T12]("12"), newDependency[T13]("13"), newDependency[T14]("14"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10), InitializedDouble(value11), InitializedDouble(value12), InitializedDouble(value13), InitializedDouble(value14)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"), newDependency[T11]("11"), newDependency[T12]("12"), newDependency[T13]("13"), newDependency[T14]("14"), newDependency[T15]("15"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10), InitializedDouble(value11), InitializedDouble(value12), InitializedDouble(value13), InitializedDouble(value14), InitializedDouble(value15)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"), newDependency[T11]("11"), newDependency[T12]("12"), newDependency[T13]("13"), newDependency[T14]("14"), newDependency[T15]("15"), newDependency[T16]("16"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10), InitializedDouble(value11), InitializedDouble(value12), InitializedDouble(value13), InitializedDouble(value14), InitializedDouble(value15), InitializedDouble(value16)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, T17: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16], Dependency[T17]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"), newDependency[T11]("11"), newDependency[T12]("12"), newDependency[T13]("13"), newDependency[T14]("14"), newDependency[T15]("15"), newDependency[T16]("16"), newDependency[T17]("17"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16), state.get(dep17)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10), InitializedDouble(value11), InitializedDouble(value12), InitializedDouble(value13), InitializedDouble(value14), InitializedDouble(value15), InitializedDouble(value16), InitializedDouble(value17)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16, double17) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16, double17)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, T17: ClassTag, T18: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16], Dependency[T17], Dependency[T18]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"), newDependency[T11]("11"), newDependency[T12]("12"), newDependency[T13]("13"), newDependency[T14]("14"), newDependency[T15]("15"), newDependency[T16]("16"), newDependency[T17]("17"), newDependency[T18]("18"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16), state.get(dep17), state.get(dep18)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10), InitializedDouble(value11), InitializedDouble(value12), InitializedDouble(value13), InitializedDouble(value14), InitializedDouble(value15), InitializedDouble(value16), InitializedDouble(value17), InitializedDouble(value18)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16, double17, double18) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16, double17, double18)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, T17: ClassTag, T18: ClassTag, T19: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16], Dependency[T17], Dependency[T18], Dependency[T19]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"), newDependency[T11]("11"), newDependency[T12]("12"), newDependency[T13]("13"), newDependency[T14]("14"), newDependency[T15]("15"), newDependency[T16]("16"), newDependency[T17]("17"), newDependency[T18]("18"), newDependency[T19]("19"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16), state.get(dep17), state.get(dep18), state.get(dep19)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10), InitializedDouble(value11), InitializedDouble(value12), InitializedDouble(value13), InitializedDouble(value14), InitializedDouble(value15), InitializedDouble(value16), InitializedDouble(value17), InitializedDouble(value18), InitializedDouble(value19)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18, value19))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16, double17, double18, double19) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16, double17, double18, double19)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, T17: ClassTag, T18: ClassTag, T19: ClassTag, T20: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16], Dependency[T17], Dependency[T18], Dependency[T19], Dependency[T20]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19, dep20) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"), newDependency[T11]("11"), newDependency[T12]("12"), newDependency[T13]("13"), newDependency[T14]("14"), newDependency[T15]("15"), newDependency[T16]("16"), newDependency[T17]("17"), newDependency[T18]("18"), newDependency[T19]("19"), newDependency[T20]("20"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16), state.get(dep17), state.get(dep18), state.get(dep19), state.get(dep20)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10), InitializedDouble(value11), InitializedDouble(value12), InitializedDouble(value13), InitializedDouble(value14), InitializedDouble(value15), InitializedDouble(value16), InitializedDouble(value17), InitializedDouble(value18), InitializedDouble(value19), InitializedDouble(value20)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18, value19, value20))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16, double17, double18, double19, double20) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16, double17, double18, double19, double20)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19, dep20)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, T17: ClassTag, T18: ClassTag, T19: ClassTag, T20: ClassTag, T21: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16], Dependency[T17], Dependency[T18], Dependency[T19], Dependency[T20], Dependency[T21]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19, dep20, dep21) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"), newDependency[T11]("11"), newDependency[T12]("12"), newDependency[T13]("13"), newDependency[T14]("14"), newDependency[T15]("15"), newDependency[T16]("16"), newDependency[T17]("17"), newDependency[T18]("18"), newDependency[T19]("19"), newDependency[T20]("20"), newDependency[T21]("21"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16), state.get(dep17), state.get(dep18), state.get(dep19), state.get(dep20), state.get(dep21)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10), InitializedDouble(value11), InitializedDouble(value12), InitializedDouble(value13), InitializedDouble(value14), InitializedDouble(value15), InitializedDouble(value16), InitializedDouble(value17), InitializedDouble(value18), InitializedDouble(value19), InitializedDouble(value20), InitializedDouble(value21)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18, value19, value20, value21))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16, double17, double18, double19, double20, double21) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16, double17, double18, double19, double20, double21)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19, dep20, dep21)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, T17: ClassTag, T18: ClassTag, T19: ClassTag, T20: ClassTag, T21: ClassTag, T22: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16], Dependency[T17], Dependency[T18], Dependency[T19], Dependency[T20], Dependency[T21], Dependency[T22]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19, dep20, dep21, dep22) = (newDependency[T1]("1"), newDependency[T2]("2"), newDependency[T3]("3"), newDependency[T4]("4"), newDependency[T5]("5"), newDependency[T6]("6"), newDependency[T7]("7"), newDependency[T8]("8"), newDependency[T9]("9"), newDependency[T10]("10"), newDependency[T11]("11"), newDependency[T12]("12"), newDependency[T13]("13"), newDependency[T14]("14"), newDependency[T15]("15"), newDependency[T16]("16"), newDependency[T17]("17"), newDependency[T18]("18"), newDependency[T19]("19"), newDependency[T20]("20"), newDependency[T21]("21"), newDependency[T22]("22"))
    val calcResult = {state:Environment =>
      (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16), state.get(dep17), state.get(dep18), state.get(dep19), state.get(dep20), state.get(dep21), state.get(dep22)) match {
        case (InitializedDouble(value1), InitializedDouble(value2), InitializedDouble(value3), InitializedDouble(value4), InitializedDouble(value5), InitializedDouble(value6), InitializedDouble(value7), InitializedDouble(value8), InitializedDouble(value9), InitializedDouble(value10), InitializedDouble(value11), InitializedDouble(value12), InitializedDouble(value13), InitializedDouble(value14), InitializedDouble(value15), InitializedDouble(value16), InitializedDouble(value17), InitializedDouble(value18), InitializedDouble(value19), InitializedDouble(value20), InitializedDouble(value21), InitializedDouble(value22)) => Right(resultExpression(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18, value19, value20, value21, value22))
        case (double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16, double17, double18, double19, double20, double21, double22) => Left(reportMissing(Seq(double1, double2, double3, double4, double5, double6, double7, double8, double9, double10, double11, double12, double13, double14, double15, double16, double17, double18, double19, double20, double21, double22)))
      }
    }
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19, dep20, dep21, dep22)(when).results
  }

}
