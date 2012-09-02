package net.rafaelferreira.goose

import org.specs2.Specification
import org.specs2.matcher._
import org.specs2.execute._


class GooseSpec extends Specification with ResultMatchers with Goose {
    def is = "variable assumption passing"      ^ e1 ^
             "variable assumption failing"      ^ e2 ^ 
             "stub assumption passing"          ^ e3 ^
             "stub assumption failing"          ^ e4 ^
             "mixed assumptions passing"        ^ e5 ^
             "variable assumption overriding"   ^ e6 ^ 
             "mock assumption overriding"       ^ e7 ^
             "checking in outer and in inner"   ^ e8 ^
             "outer passing and inner failing"  ^ e9 ^
             "inner passing and outer failing"  ^ e10 ^
             "arity 1 passing"  ^ e11 ^
             "arity 1 failing"  ^ e12 ^
             "arity 3 passing"  ^ e13 ^
             "arity 3 failing"  ^ e14 ^
             "fails when variable is left undefined" ^ e15 ^
             end
  
  def e1 = 
    check({(x:String,y:String) => x+y}) {(value, y) => 
      _.when(value ==> "as").
        and(y ==> "df").
        then(_ must_== "asdf")
    }
  
  def e2 = {
    val fragments = check({ (x: String, y: String) => x + y }) { (value, y) =>
      _.when(value ==> "xx").
        when(y ==> "xx").
        then(_ must_== "asdf")
    }
    fragments.examples.head.execute must be failing
  }
    
  def e3 = {
    trait Foo {def foo:String}
    trait Bar {def bar:String}
    
    check((_:Foo).foo + (_:Bar).bar) {(foo, bar) => 
      _.when(foo.stub(_.foo) ==> "as").
        and(bar.stub(_.bar) ==> "df").
        then(_ must_== "asdf")
    }
  }
  
  def e4 = {
    trait Foo {def foo:String}
    trait Bar {def bar:String}
    
    val fragments = check((_:Foo).foo + (_:Bar).bar) {(foo, bar) => 
      _.when(foo.stub(_.foo) ==> "xx").
        and(bar.stub(_.bar) ==> "xx").
        then(_ must_== "asdf")
    }
    
    fragments.examples.head.execute must be failing
  }
  
  def e5 = {
    trait Foo {def foo:String}
    
    check((_:Foo).foo + (_:String)) {(foo, str) => 
      _.when(foo.stub(_.foo) ==> "as").
        and(str ==> "df").
        then(_ must_== "asdf")
    }
  }
  
  def e6 = check((_:String)+(_:String)) {(x,y) => _.
    when(x ==> "a").
    and(y ==> "b").but {
      _.and(x ==> "R").
        then(_ must_== "Rb")
    }
  }

  def e7 = {
    trait Foo {def foo:String}
    trait Bar {def bar:String}
    
    check((_:Foo).foo + (_:Bar).bar) {(foo, bar) => 
      _.when(foo.stub(_.foo) ==> "as").
        and(bar.stub(_.bar) ==> "df").
        but {
          _.and(foo.stub(_.foo) ==> "R").
            then(_ must_== "Rdf")
        }
    }
  }
  
  def e8 = 
    check((_:String) + (_:String)) {(x, y) => 
      _.when(x ==> "as").
        and(y  ==> "df").
        then(_ must_== "asdf").
        but {
          _.and(x ==> "R").
            then(_ must_== "Rdf")
        }
    }
  
  def e9 = {
    val fragments = check((_:String) + (_:String)) {(x, y) => 
      _.when(x ==> "as").
        and(y  ==> "df").
        then(_ must_== "asdf").
        but {
          _.and(x ==> "R").
            then(_ must_== "qwerty")
        }
    }
    
    (fragments.examples(0).execute must beSuccessful) and
    (fragments.examples(1).execute must beFailing)
  }
  
  def e10 = {
    val fragments = check((_:String) + (_:String)) {(x, y) => 
      _.when(x ==> "as").
        and(y  ==> "df").
        then(_ must_== "qwerty").
        but {
          _.and(x ==> "R").
            then(_ must_== "Rdf")
        }
    }
    
    (fragments.examples(1).execute must beSuccessful) and
    (fragments.examples(0).execute must beFailing)
  }
  
  
  def e11 =
    check(identity[String] _) {x => 
      _.when(x ==> "as").
        then(_ must_== "as")
    }
    
 def e12 = {
   val fragments = check(identity[String] _) {x => 
      _.when(x ==> "as").
        then(_ must_== "zz")
    }
   fragments.examples.head.execute must beFailing
 } 
 
 def e13 =
    check((_:String)+(_:String)+(_:String)) {(x, y, z) => 
      _.when(x ==> "a").
        and(y ==> "b").
        and(z ==> "c").
        then(_ must_== "abc")
    }
 
 def e14 = {
   val fragments = check((_:String)+(_:String)+(_:String)) {(x, y, z) => 
      _.when(x ==> "a").
        and(y ==> "b").
        and(z ==> "c").
        then(_ must_== "zzz")
    }
   fragments.examples.head.execute must beFailing
 }
 
 def e15 = {
   val fragments = check(identity[String] _) {x =>
     _.then(_ must_== "10")
   }
   fragments.examples.head.execute must beFailing
 }

}