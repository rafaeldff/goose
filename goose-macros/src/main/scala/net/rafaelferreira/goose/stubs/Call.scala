package net.rafaelferreira.goose
package stubs

import scala.language.experimental.macros

case class Call(method:String, args:Seq[Any])

object Call {
  def capture[T](methodCall: T => Any): Call = macro CallMacro.capture_impl[T]
}
