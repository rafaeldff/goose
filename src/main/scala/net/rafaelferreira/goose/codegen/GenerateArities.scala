package net.rafaelferreira.goose.codegen

trait MiniTemplater {
  /**
   * Will look for substrings delimited by '%'s and replace them with n copies
   * of the substring with the index of the copy substituted for #i, interpolated with
   * the given separator string (by default it is a comma).
   * 
   * For instance:
   *   expand("%#i bottles of beer", 3) 
   * will expand to
   *   "1 bottles of beer, 2 bottles of beer, 3 bottles of beer"
   *   
   */
  def expand(template:String, n:Int, separator:String=", ") = {
    val TemplateExpression = """%([^%]*)%""".r
    TemplateExpression.replaceAllIn(template, {m =>
      val expression:String = m.group(1)
      (1 to n).map(i => expression.replaceAll("#i", i.toString)).mkString(separator)
    })  
  }  
}

object GenerateArities extends App with MiniTemplater {
  import scalax.io._
  import scalax.file.Path
  
  val numberOfMethods = 3
  
  val destinationFile = Path("src", "main", "scala", "net", "rafaelferreira", "goose", "CheckingForVariousArities.scala").createFile(failIfExists=false)
  
  val checkMethod = 
       """|  def check[%T#i: ClassTag%, R](resultExpression: (%T#i%) => R)(testDefinition: (%Dependency[T#i]%) => When[R] => When[R]): Fragments = {
          |    val (%dep#i%) = (%newDependency[T#i]("#i")%)
          |    val calcResult = {state:Environment =>
          |      (%state.get(dep#i)%) match {
          |        case (%InitializedDouble(value#i)%) => Right(resultExpression(%value#i%))
          |        case (%double#i%) => Left(reportMissing(Seq(%double#i%)))
          |      }
          |    }
          |    val when = new When[R](calcResult)
          |    testDefinition(%dep#i%)(when).results
          |  }
          |""".stripMargin
  
  def generatedCode = {
    val methods = (1 to numberOfMethods).map(expand(checkMethod, _))
    val body = methods.mkString("\n")
    
    """|package net.rafaelferreira.goose
       |import scala.reflect.ClassTag
       |
       |import org.specs2.specification.Fragments
       |import org.specs2.Specification
       |
       |
       |trait CheckingForVariousArities extends CheckHelpers {self: GooseStructure with Specification =>
       |%s
       |}
       |""".stripMargin.format(body) 
  }
  
  destinationFile.write(generatedCode)
}
