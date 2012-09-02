package net.rafaelferreira.goose

trait CheckHelpers {self: GooseStructure =>  
  def whenAllPresent[R](vs: Seq[Option[Any]])(result: => R) = {
    val missingValues = vs.zipWithIndex.collect {case (None, i) => i}
    missingValues match {
      case Nil => Right(result)
      case missing @ _ => Left("No value was supplied for dependencies %s. Did you forget 'when' or 'and' clauses?" format (missing.mkString("[", ",", "]")))
    }
  }
}
