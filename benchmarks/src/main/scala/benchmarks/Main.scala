package benchmarks

import java.lang.System.exit

object Main {
  def main(args: Array[String]): Unit = {
    val benchmarks = Seq(new dispatch.StaticCallBenchmark()) //Discover.discovered

    val format = args.lift(0).map(Format(_)).getOrElse(TextFormat)
    val results = benchmarks.map { bench =>
      bench.loop(args.lift(0).map(_.toInt).getOrElse(bench.iterations))
    }
    val success = results.forall(_.success)

    println(format.show(results))

    if (!success) exit(1)
  }
}
