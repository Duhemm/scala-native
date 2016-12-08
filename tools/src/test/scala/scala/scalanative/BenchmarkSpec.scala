package scala.scalanative

import java.io.File

import optimizer.Driver
import tools.Config

abstract class BenchmarkSpec extends BinarySpec {

  case class BenchmarkResult(minNs: Long, maxNs: Long, avgNs: Long, output: String) {
    override def toString(): String = {
      def toMs(ns: Long): Double = ns.toDouble / 1000.0
      val format = new java.text.DecimalFormat("#.###")
      val minMs = format.format(toMs(minNs))
      val maxMs = format.format(toMs(maxNs))
      val avgMs = format.format(toMs(avgNs))
      "-" * 181 +
        s"""min = ${minMs}ms, max = ${maxMs}ms, avg = ${avgMs}ms,
           |output =
           |$output
           |""".stripMargin +
           "-" * 181
    }
  }

  private def timed[T](op: => T): (T, Long) = {
    val startTime = System.nanoTime()
    val result    = op
    val totalTime = System.nanoTime - startTime
    (result, totalTime)
  }

  private def run(iterations: Int, binary: File): BenchmarkResult = {
    var minNs        = Long.MaxValue
    var maxNs        = Long.MinValue
    val (out, total) = timed {
      val out = collection.mutable.Buffer.empty[String]
      for { _ <- 1 to iterations } {
        val (outPart, time) = timed { run(binary) { case (_, out, _) => out.mkString("\n") } }
        minNs               = minNs min time
        maxNs               = maxNs max time
        out += outPart
      }
      out.mkString("\n")
    }
    BenchmarkResult(minNs, maxNs, total, out)
  }

  private def makeMain(entry: String,
                       iterations: Int): String = {

    val call = s"${entry.substring(0, entry.length - 1)}.main(args)\n" * iterations
    s"""object Benchmark {
       |  def main(args: Array[String]): Unit = {
       |    $call
       |  }
       |}""".stripMargin
  }

  def benchmark[T](entry: String,
                   driver: Driver,
                   configFn: Config => Config,
                   iterations: Int,
                   sources: Map[String, String],
                   linkage: Map[String, String] = Map.empty,
                   opts: Seq[String] = defaultClangOptions)(fn: BenchmarkResult => T): T = {
    val newSources =
      sources + ("Benchmark.scala" -> makeMain(entry, iterations))
    val newEntry =
      "Benchmark$"

    val result =
      makeBinary(newEntry, newSources, driver, configFn, linkage, opts) {
        case (_, _, binary) => run(1, binary)
      }

    fn(result)
  }

  def compare[T](entry: String,
                 setups: Seq[(Driver, Config => Config)],
                 iterations: Int,
                 sources: Map[String, String],
                 linkage: Map[String, String] = Map.empty,
                 opts: Seq[String] = defaultClangOptions)(fn: Seq[BenchmarkResult] => T): T = {

    val results =
      setups map { case (driver, configFn) =>
        benchmark(entry, driver, configFn, iterations, sources, linkage, opts)(identity)
      }

    fn(results)
  }

}
