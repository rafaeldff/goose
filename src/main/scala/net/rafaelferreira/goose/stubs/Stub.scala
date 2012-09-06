package net.rafaelferreira.goose.stubs

class Stub[T: ClassManifest] {
    def expecting[R](call: T => R, result: R):Stub[T] = this
    def stubObject: T = sys.error("todo ")
}