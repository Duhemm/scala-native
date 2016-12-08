package scala.scalanative
package optimizer
package pass

import org.scalatest._

class CopyPropagationBenchmark extends BenchmarkSpec with Matchers {

  "Copy propagation" should "not degrade performance" in {
    val baseDriver     = Driver().remove(GlobalBoxingElimination)
    val improvedDriver = Driver()

    benchmark("A$",
              baseDriver,
              improvedDriver,
              20,
              """object A {
                |  def main(args: Array[String]): Unit =
                |    println("Hello, world!")
                |}""".stripMargin) {
      case (base, improved) =>
        improved.avgNs should be < base.avgNs
    }
  }
}
