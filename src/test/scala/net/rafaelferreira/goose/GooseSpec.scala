package net.rafaelferreira.goose

import org.specs2.Specification
import org.specs2.matcher._
import org.specs2.execute._


class GooseSpec extends Specification with ResultMatchers with Goose {
    def is = args.report(failtrace=true) ^
             "variable assumption passing"      ^ e1 ^
             "variable assumption failing"      ^ e2 ^ 
             "stub assumption passing"          ^ e3 ^
             "stub assumption failing"          ^ e4 ^
             "allows stub expectation results to be other dependencies" ^ e18 ^ e19 ^e20 ^
             "allows stub expectation arguments to refer to other dependencies" ^ e21 ^
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
             "fails when variable is left undefined" ^ e15 ^ e16 ^ e17 ^
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
      _.when(foo(_.foo) ==> "as").
        and(bar(_.bar) ==> "df").
        then(_ must_== "asdf")
    }
  }
  
  def e4 = {
    trait Foo {def foo:String}
    trait Bar {def bar:String}
    
    val fragments = check((_:Foo).foo + (_:Bar).bar) {(foo, bar) => 
      _.when(foo(_.foo) ==> "xx").
        and(bar(_.bar) ==> "xx").
        then(_ must_== "asdf")
    }
    
    fragments.examples.head.execute must be failing
  }
  
  def e5 = {
    trait Foo {def foo:String}
    
    check((_:Foo).foo + (_:String)) {(foo, str) => 
      _.when(foo(_.foo) ==> "as").
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
      _.when(foo(_.foo) ==> "as").
        and(bar(_.bar) ==> "df").
        but {
          _.and(foo(_.foo) ==> "R").
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
   val fragments = check { (_:String,_:String) } {(x, y) =>
     _.then(_ must_== ("a", "b"))
   }
   fragments.examples.head.execute must beFailing
 }
 
 def e16 = {
   val fragments = check(identity[String] _) {x =>
     _.then(_ must_== "10")
   }
   fragments.examples.head.execute must beFailing
 }
 
 def e17 = {
   val fragments = check { (_:String,_:String, _:String) } { (x,y,z) =>
     _.when(x ==> "a").then(_ must_== ("a", "b", "c"))
   }
   fragments.examples.head.execute must beFailing
 }
 
 def e18 = {
    trait Bar { def makeBaz:Baz }
    trait Baz { def bazz:String }

    check((bar:Bar, baz:Baz) => bar.makeBaz.bazz) { (bar, baz) =>
      _.when(baz(_.bazz) ==> "called bazz!").
        and(bar(_.makeBaz) ==> baz).
        then(_ must_== "called bazz!")
    }
  }
 
 def e19 = {
    trait Bar { def makeBaz:Baz }
    trait Baz { def bazz:String }

    check((bar:Bar, baz:Baz) => bar.makeBaz.bazz) { (bar, baz) =>
      _.and(bar(_.makeBaz) ==> baz).
        when(baz(_.bazz) ==> "called bazz!").
        then(_ must_== "called bazz!")
    }
  }

  def e20 = {
    class Foo { def op(baz:Baz) =  baz.bazz }
    trait Bar { def makeBaz:Baz }
    trait Baz { def bazz:String }

    check((bar:Bar, baz:Baz) => (new Foo).op(bar.makeBaz)) { (bar, baz) =>
      _.when(baz(_.bazz) ==> "called bazz!").
        and(bar(_.makeBaz) ==> baz).
        then(_ must_== "called bazz!")
    }
  }
  
  def e21 = {
    trait Bar {}
    trait Foo { def foo(bar:Bar):String }
    val barObject = new Bar {}
    check((_:Foo).foo((_:Bar))) {(foo, bar) =>
      _.when(bar ==> barObject).
        and(foo(_.foo(bar)) ==> "42").
        then(_ must_== "42")
    }
  }
}

















