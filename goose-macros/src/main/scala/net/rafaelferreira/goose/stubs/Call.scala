package net.rafaelferreira.goose
package stubs

import scala.language.experimental.macros

case class Call[T](context:Any, method:String, args:Seq[Any])

object Call {
}
