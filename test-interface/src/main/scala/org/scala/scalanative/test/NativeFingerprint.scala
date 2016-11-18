package scala.scalanative
package test

import sbt.testing.SubclassFingerprint

case object NativeFingerprint extends SubclassFingerprint {

  override def isModule(): Boolean = false

  override def superclassName(): String = "scala.scalanative.test.Test"

  override def requireNoArgConstructor(): Boolean = false

}
