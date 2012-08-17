package net.rafaelferreira.goose

import org.specs2.Specification
import org.specs2.matcher._
import org.specs2.execute._


class GooseSpec extends Specification with ResultMatchers with Goose {
  def is = `variable assumption passing` ^ `variable assumption failing`
  
  def `variable assumption passing` =
    check({(x:String,y:String) => x+y}) {(value, y) => 
      _.when(value ==> "as").
        when(y ==> "df").
        then(_ must_== "asdf")
    }

  def `variable assumption failing` = {
    val fragments = check({ (x: String, y: String) => x + y }) { (value, y) =>
      _.when(value ==> "xx").
        when(y ==> "xx").
        then(_ must_== "asdf")
    }
    fragments.examples.head.execute must be failing
  }
}