package net.rafaelferreira.goose
import scala.reflect.ClassTag

import org.specs2.specification.Fragments
import org.specs2.Specification


trait CheckingForVariousArities extends CheckHelpers {self: GooseStructure with Specification =>
  def check[T1: ClassTag, R](resultExpression: (T1) => R)(testDefinition: (Dependency[T1]) => When[R] => When[R]): Fragments = {
    val (dep1) = (dep[T1])
    val calcResult = {state:State =>
      val (value1) = (state.get(dep1))
      whenAllPresent(Seq(value1)) { resultExpression(value1.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, R](resultExpression: (T1, T2) => R)(testDefinition: (Dependency[T1], Dependency[T2]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2) = (dep[T1], dep[T2])
    val calcResult = {state:State =>
      val (value1, value2) = (state.get(dep1), state.get(dep2))
      whenAllPresent(Seq(value1, value2)) { resultExpression(value1.get, value2.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, R](resultExpression: (T1, T2, T3) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3) = (dep[T1], dep[T2], dep[T3])
    val calcResult = {state:State =>
      val (value1, value2, value3) = (state.get(dep1), state.get(dep2), state.get(dep3))
      whenAllPresent(Seq(value1, value2, value3)) { resultExpression(value1.get, value2.get, value3.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, R](resultExpression: (T1, T2, T3, T4) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4) = (dep[T1], dep[T2], dep[T3], dep[T4])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4))
      whenAllPresent(Seq(value1, value2, value3, value4)) { resultExpression(value1.get, value2.get, value3.get, value4.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5))
      whenAllPresent(Seq(value1, value2, value3, value4, value5)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10], dep[T11])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get, value11.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10], dep[T11], dep[T12])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get, value11.get, value12.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10], dep[T11], dep[T12], dep[T13])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get, value11.get, value12.get, value13.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10], dep[T11], dep[T12], dep[T13], dep[T14])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get, value11.get, value12.get, value13.get, value14.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10], dep[T11], dep[T12], dep[T13], dep[T14], dep[T15])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get, value11.get, value12.get, value13.get, value14.get, value15.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10], dep[T11], dep[T12], dep[T13], dep[T14], dep[T15], dep[T16])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get, value11.get, value12.get, value13.get, value14.get, value15.get, value16.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, T17: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16], Dependency[T17]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10], dep[T11], dep[T12], dep[T13], dep[T14], dep[T15], dep[T16], dep[T17])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16), state.get(dep17))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get, value11.get, value12.get, value13.get, value14.get, value15.get, value16.get, value17.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, T17: ClassTag, T18: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16], Dependency[T17], Dependency[T18]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10], dep[T11], dep[T12], dep[T13], dep[T14], dep[T15], dep[T16], dep[T17], dep[T18])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16), state.get(dep17), state.get(dep18))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get, value11.get, value12.get, value13.get, value14.get, value15.get, value16.get, value17.get, value18.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, T17: ClassTag, T18: ClassTag, T19: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16], Dependency[T17], Dependency[T18], Dependency[T19]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10], dep[T11], dep[T12], dep[T13], dep[T14], dep[T15], dep[T16], dep[T17], dep[T18], dep[T19])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18, value19) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16), state.get(dep17), state.get(dep18), state.get(dep19))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18, value19)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get, value11.get, value12.get, value13.get, value14.get, value15.get, value16.get, value17.get, value18.get, value19.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, T17: ClassTag, T18: ClassTag, T19: ClassTag, T20: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16], Dependency[T17], Dependency[T18], Dependency[T19], Dependency[T20]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19, dep20) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10], dep[T11], dep[T12], dep[T13], dep[T14], dep[T15], dep[T16], dep[T17], dep[T18], dep[T19], dep[T20])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18, value19, value20) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16), state.get(dep17), state.get(dep18), state.get(dep19), state.get(dep20))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18, value19, value20)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get, value11.get, value12.get, value13.get, value14.get, value15.get, value16.get, value17.get, value18.get, value19.get, value20.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19, dep20)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, T17: ClassTag, T18: ClassTag, T19: ClassTag, T20: ClassTag, T21: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16], Dependency[T17], Dependency[T18], Dependency[T19], Dependency[T20], Dependency[T21]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19, dep20, dep21) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10], dep[T11], dep[T12], dep[T13], dep[T14], dep[T15], dep[T16], dep[T17], dep[T18], dep[T19], dep[T20], dep[T21])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18, value19, value20, value21) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16), state.get(dep17), state.get(dep18), state.get(dep19), state.get(dep20), state.get(dep21))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18, value19, value20, value21)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get, value11.get, value12.get, value13.get, value14.get, value15.get, value16.get, value17.get, value18.get, value19.get, value20.get, value21.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19, dep20, dep21)(when).results
  }

  def check[T1: ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag, T5: ClassTag, T6: ClassTag, T7: ClassTag, T8: ClassTag, T9: ClassTag, T10: ClassTag, T11: ClassTag, T12: ClassTag, T13: ClassTag, T14: ClassTag, T15: ClassTag, T16: ClassTag, T17: ClassTag, T18: ClassTag, T19: ClassTag, T20: ClassTag, T21: ClassTag, T22: ClassTag, R](resultExpression: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R)(testDefinition: (Dependency[T1], Dependency[T2], Dependency[T3], Dependency[T4], Dependency[T5], Dependency[T6], Dependency[T7], Dependency[T8], Dependency[T9], Dependency[T10], Dependency[T11], Dependency[T12], Dependency[T13], Dependency[T14], Dependency[T15], Dependency[T16], Dependency[T17], Dependency[T18], Dependency[T19], Dependency[T20], Dependency[T21], Dependency[T22]) => When[R] => When[R]): Fragments = {
    val (dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19, dep20, dep21, dep22) = (dep[T1], dep[T2], dep[T3], dep[T4], dep[T5], dep[T6], dep[T7], dep[T8], dep[T9], dep[T10], dep[T11], dep[T12], dep[T13], dep[T14], dep[T15], dep[T16], dep[T17], dep[T18], dep[T19], dep[T20], dep[T21], dep[T22])
    val calcResult = {state:State =>
      val (value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18, value19, value20, value21, value22) = (state.get(dep1), state.get(dep2), state.get(dep3), state.get(dep4), state.get(dep5), state.get(dep6), state.get(dep7), state.get(dep8), state.get(dep9), state.get(dep10), state.get(dep11), state.get(dep12), state.get(dep13), state.get(dep14), state.get(dep15), state.get(dep16), state.get(dep17), state.get(dep18), state.get(dep19), state.get(dep20), state.get(dep21), state.get(dep22))
      whenAllPresent(Seq(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12, value13, value14, value15, value16, value17, value18, value19, value20, value21, value22)) { resultExpression(value1.get, value2.get, value3.get, value4.get, value5.get, value6.get, value7.get, value8.get, value9.get, value10.get, value11.get, value12.get, value13.get, value14.get, value15.get, value16.get, value17.get, value18.get, value19.get, value20.get, value21.get, value22.get) }
    }
    
    val when = new When[R](calcResult)
    testDefinition(dep1, dep2, dep3, dep4, dep5, dep6, dep7, dep8, dep9, dep10, dep11, dep12, dep13, dep14, dep15, dep16, dep17, dep18, dep19, dep20, dep21, dep22)(when).results
  }

}
