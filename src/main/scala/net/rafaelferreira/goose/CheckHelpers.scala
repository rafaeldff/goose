package net.rafaelferreira.goose

trait CheckHelpers {self: GooseSpecificationDSL =>  
  import PartialFunction._
  
  def reportMissing(vs: Seq[TestDouble[_]]) = {
    val missing = vs.zipWithIndex.filterNot {cond(_){case (InitializedDouble(_), i) => true}}
    "No value was supplied for dependencies %s. Did you forget 'when' or 'and' clauses?" format (missing.mkString("[", ",", "]"))
  }
  
  def whenAllPresent[R](vs: Seq[TestDouble[Any]])(result: => R) = {
    val missingValues = vs.zipWithIndex.collect {case (UninitializedDouble, i) => i}
    missingValues match {
      case Nil => Right(result)
      case missing @ _ => Left("No value was supplied for dependencies %s. Did you forget 'when' or 'and' clauses?" format (missing.mkString("[", ",", "]")))
    }
  }
}

