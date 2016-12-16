package benchmarks

import java.lang.System.exit

object Main {
  def main(args: Array[String]): Unit = {
    val benchmarks = Seq( // working    : deltablue, richards, havlak, list, mandelbrot, permute, storage
                          // not working: bounce, nbody, sieve, tracer 
      new deltablue.DeltaBlueBenchmark(),
      new richards.RichardsBenchmark(),
      new havlak.HavlakBenchmark(),
      new list.ListBenchmark(),
      new mandelbrot.MandelbrotBenchmark(),
      new permute.PermuteBenchmark(),
      //new storage.StorageBenchmark(),
      new towers.TowersBenchmark()
    ) //Discover.discovered

    val format = args.lift(0).map(Format(_)).getOrElse(TextFormat)
    val results = benchmarks.map { bench =>
      bench.loop(bench.iterations)
    }
    val success = results.forall(_.success)

    println(format.show(results))

    if (!success) exit(1)
  }
}
