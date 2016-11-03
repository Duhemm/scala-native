package scala.scalanative
package testing
package compiler

import org.scalatest._

class CompilerTest extends FlatSpec with Matchers with Inspectors {

  "The compiler" should "be able to get NIR files"in {
    val files = Compiler { _ getNIR "class A" }
    files should have length 1
    files(0).getName should be ("A.hnir")
  }

  it should "compile whole directories" in {
    val sources = Map(
      "A.scala" -> "class A",
      "B.scala" -> "class B extends A",
      "C.scala" -> "trait C",
      "D.scala" -> """class D extends B with C
                     |object E""".stripMargin
    )

    Compiler.withSources(sources) {
      case (sourcesDir, compiler) =>
        val nirFiles = compiler.getNIR(sourcesDir) map (_.getName)
        val expectedNames =
          Seq("E$.hnir", "A.hnir", "B.hnir", "C.hnir", "D.hnir")
        nirFiles should contain theSameElementsAs expectedNames
    }
  }

  it should "report compilation errors" in {
    assertThrows[api.CompilationFailedException] {
      Compiler { _ getNIR "invalid" }
    }
  }

  it should "compile to a specified directory"in {
    val temporaryDir = java.nio.file.Files.createTempDirectory("my-target").toFile()
    val nirFiles = Compiler(outDir = temporaryDir) { _ getNIR "class A" }
    forAll (nirFiles) { _.getParentFile should be(temporaryDir) }
  }

}
