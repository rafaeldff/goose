package net.rafaelferreira.goose
package stubs

import org.specs2.Specification

trait Stubs { this: GooseStructure =>
  private[Stubs] val mocker = new org.specs2.mock.MockitoMocker {}
  
  class Stubbing[T,R](dependency: GeneralDependency[T])(call: T => R) {
      def ==>(r: R) = new Assumption[T] {
        def relatedTo = dependency
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

  trait StubDependency[T] { self: GeneralDependency[T] =>
    val manifest: ClassManifest[T]

    def stub[R](call: T => R) = new Stubbing[T,R](this)(call)
  }
}