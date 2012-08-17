package net.rafaelferreira.goose

import org.specs2.Specification
import org.specs2.matcher._
import org.specs2.execute._


class GooseSpec extends Specification with ResultMatchers with Goose {
  def is = "Specifying goose itself" ^
            `variable assumption passing` ^
            `variable assumption failing` ^ 
            `stub assumption passing`
  
  def `variable assumption passing` = "variable assumption passing" ^
    check({(x:String,y:String) => x+y}) {(value, y) => 
      _.when(value ==> "as").
        and(y ==> "df").
        then(_ must_== "asdf")
    }
  
  def `variable assumption failing` = "variable assumption failing" ^{
    val fragments = check({ (x: String, y: String) => x + y }) { (value, y) =>
      _.when(value ==> "xx").
        when(y ==> "xx").
        then(_ must_== "asdf")
    }
    fragments.examples.head.execute must be failing
  }
    
  def `stub assumption passing` = "stub assumption passing" ^{
    trait Foo {def foo:String}
    trait Bar {def bar:String}
    
    check((_:Foo).foo + (_:Bar).bar) {(foo, bar) => 
      _.when(foo.stub(_.foo) ==> "as").
        and(bar.stub(_.bar) ==> "df").
        then(_ must_== "asdf")
    }
  }


}