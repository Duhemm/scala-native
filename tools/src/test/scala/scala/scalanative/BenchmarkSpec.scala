package scala.scalanative

import java.io.File

import optimizer.Driver

abstract class BenchmarkSpec extends BinarySpec {

  case class BenchmarkResult(minNs: Long, maxNs: Long, avgNs: Long) {
    override def toString(): String = {
      def toMs(ns: Long): Double = ns.toDouble / 1000.0
      val format = new java.text.DecimalFormat("#.###")
      val minMs = format.format(toMs(minNs))
      val maxMs = format.format(toMs(maxNs))
      val avgMs = format.format(toMs(avgNs))
      s"min = ${minMs}ms, max = ${maxMs}ms, avg = ${avgMs}ms"
    }
  }

  private def timed[T](op: => T): Long = {
    val startTime = System.nanoTime()
    val _         = op
    System.nanoTime - startTime
  }

  private def run(iterations: Int, binary: File): BenchmarkResult = {
    var minNs     = Long.MaxValue
    var maxNs     = Long.MinValue
    val total = timed {
      for { _ <- 1 to iterations } {
        val time = timed { run(binary) { case _ => () } }
        minNs = minNs min time
        maxNs = maxNs max time
      }
    }
    BenchmarkResult(minNs, maxNs, total)
  }

  private def makeMain(entry: String,
                       iterations: Int): String = {
    val call = "A.main(args)\n" * iterations
    s"""object Benchmark {
       |  def main(args: Array[String]): Unit = {
       |    $call
       |  }
       |}""".stripMargin
  }

  def benchmark[T](entry: String,
                   baseDriver: Driver,
                   improvedDriver: Driver,
                   iterations: Int,
                   sources: Map[String, String],
                   linkage: Map[String, String] = Map.empty,
                   opts: Seq[String] = defaultClangOptions)(fn: (BenchmarkResult, BenchmarkResult) => T): T = {

    val newSources =
      sources + ("Benchmark.scala" -> makeMain(xentry, iterations))
    val newEntry =
      "Benchmark$"

    val baseResult =
      makeBinary(newEntry, newSources, baseDriver, linkage, opts) {
        case (_, _, baseBinary) => run(iterations, baseBinary)
      }

    val improvedResult =
      makeBinary(newEntry, newSources, improvedDriver, linkage, opts) {
        case (_, _, improvedBinary) => run(iterations, improvedBinary)
      }

    fn(baseResult, improvedResult)
  }

}
