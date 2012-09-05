package net.rafaelferreira.goose

import org.specs2.Specification

trait Stubs { this: GooseStructure =>
  val mocker = new org.specs2.mock.MockitoMocker {}

  trait StubDependency[T] { self: GeneralDependency[T] =>
    val manifest: ClassManifest[T]
    
    class Stubbing[R](call: T => R) {
      def ==>(r: R) = new Assumption[T] {
        def relatedTo = StubDependency.this
        def apply(previous: Option[T]) = {
          val mock = previous match {
            case None => mocker.mock(manifest)
            case Some(mock) => mock
          }
          mocker.when(call(mock)).thenReturn(r)
          Some(mock)
        }
      }
    }

    def stub[R](call: T => R) = new Stubbing[R](call)
  }
}