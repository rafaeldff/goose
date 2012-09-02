package net.rafaelferreira.goose.codegen

object GenerateArities extends App {
  def expand(template:String, n:Int) = {
    val TemplateExpression = """%([^%]*)%""".r
    TemplateExpression.replaceAllIn(template, {m =>
      val expression:String = m.group(1)
      (1 to n).map(i => expression.replaceAll("#i", i.toString)).mkString(", ")
    })  
  }
  
  val checkMethod = """
    def check[%T#i: ClassManifest%, R](resultExpression: (%T#i%) => R)(testDefinition: (%Dependency[T#i]%) => When[R] => When[R]): Fragments = {
      val (%dep#i%) = (%dep[T#i]%)
      val calcResult = {state:State =>
        val (%value#i%) = (%state.get(dep#i)%)
        whenAllPresent(Seq(%value#i%)) { resultExpression(%value#i.get%) }
      }
      
      val when = new When[R](calcResult)
      testDefinition(%dep#i%)(when).results
    }
  """
  
  println(expand(checkMethod, 2))
}