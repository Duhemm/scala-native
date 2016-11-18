package scala.scalanative
package sbtplugin

import scala.scalanative.test.FakeFingerprint

import sbt._
import sbt.testing._

object TestUtilities {

  def remapTestDefinition(test: TestDefinition): TestDefinition = {
    val newFingerprint = new FakeFingerprint(test.fingerprint)
    new TestDefinition(test.name,
                       newFingerprint,
                       test.explicitlySpecified,
                       test.selectors)
  }

  def createTestMain(tests: Seq[TestDefinition]): String = {
    val cases = tests map createTestCase
    s"""object TestMain {
       |  def main(args: Array[String]): Unit = args match {
       |    ${cases mkString "\n"}
       |    case Array() =>
       |      ()
       |  }
       |}""".stripMargin
  }

  private def createTestCase(test: TestDefinition): String = {
    val body =
      test.fingerprint match {
        case fake: FakeFingerprint =>
          fake.original match {
            case NativeFingerprint() =>
              s"""new ${test.name}().test()"""
            case UTest() =>
              // s"""println("Running test ${test.name} with utest...")"""
              s"""val executor = new scala.concurrent.ExecutionContextExecutor {
                 |  override def execute(arg0: Runnable): Unit = arg0.run()
                 |  override def reportFailure(cause: Throwable): Unit = throw cause
                 |}
                 |${test.name}.tests.runAsync()(executor)""".stripMargin
            case other =>
              throw new UnsupportedOperationException(
                "Unsupported fingerprint: " + other)
          }
      }
    s"""case Array("${test.name}", rest @ _*) =>
       |  $body
       |  main(rest.toArray)""".stripMargin
  }

  private object NativeFingerprint {
    // Classloader issues?
    def unapply(fingerprint: Fingerprint): Boolean = {
      fingerprint.getClass.getName == scala.scalanative.test.NativeFingerprint.getClass.getName
    }
  }

  private object UTest {
    def unapply(fingerprint: Fingerprint): Boolean =
      fingerprint match {
        case scf: SubclassFingerprint =>
          scf.superclassName == "utest.TestSuite" &&
            scf.isModule &&
            scf.requireNoArgConstructor
        case _ => false
      }
  }

}
