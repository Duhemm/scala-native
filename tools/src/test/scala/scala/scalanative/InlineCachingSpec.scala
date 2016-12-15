package scala.scalanative

import java.io.File
import java.nio.file.Files

import optimizer.Driver
import tools.Config

abstract class InlineCachingSpec extends BenchmarkSpec {

  def easyInlineCaching[T](entry: String,
                           sources: Map[String, String])(fn: (Int, Seq[String], Seq[String]) => T): T = {
    val configForProfiling =
      (out: File) => (c: Config) => c.withProfileDispatch(true)
                                     .withProfileDispatchInfo(Some(out))

    val configForRunning =
      (out: File) => (c: Config) => {
        val parsedMap: Map[Int, Set[Int]] =
          out match {
            case f if f.exists =>
              optimizer.analysis.DispatchInfoParser(scala.io.Source.fromFile(f).mkString).mapValues(_.toSet).filter {
                case (_, vs) => vs.size <= 4
              }
                case _ => Map.empty
          }
        val solution = optimizer.analysis.Solver.solveFromProfiling(parsedMap.filter { case (k, v) => v.size < 3 }
)
        c.withProfileDispatch(false)
         .withProfileDispatchInfo(Some(out))
         .withTypeAssignments(solution)
      }
    val out = Files.createTempFile("dispatch", ".txt").toFile()

    println("#" * 181)
    println("Profiling...")
    run(entry, sources, configFn = configForProfiling(out)) { case (_, _, _) => () }

    println("#" * 181)
    println("Running...")
    run(entry, sources, configFn = configForRunning(out)) {
      case (exitCode, outLines, errLines) => fn(exitCode, outLines, errLines)
    }
  }

  def withInlineCaching[T](entry: String,
                        driver: Driver,
                        iterations: Int,
                        configFn: Config => Config = identity,
                        sources: Map[String, String],
                        linkage: Map[String, String] = Map.empty,
                        opts: Seq[String] = defaultClangOptions)(fn: BenchmarkResult => T): T = {
    val configForProfiling =
      (out: File) => configFn andThen (_.withProfileDispatch(true)
                                        .withProfileDispatchInfo(Some(out)))

    val configForBenchmarking =
      (out: File) => {
        val parsedMap: Map[Int, Set[Int]] =
          out match {
            case f if f.exists =>
              optimizer.analysis.DispatchInfoParser(scala.io.Source.fromFile(f).mkString).mapValues(_.toSet).filter {
                case (_, vs) => vs.size <= 4
              }
                case _ => Map.empty
          }
        val solution = optimizer.analysis.Solver.solveFromProfiling(parsedMap)
        configFn andThen (_.withProfileDispatchInfo(Some(out))
                           .withTypeAssignments(solution))
      }

    val withProfilingConfigs = (configFn: Config => Config) => (cfg: Config) => configFn(cfg.withProfileDispatch(true))
    val dispatchInfo = Files.createTempFile("dispatch", ".txt").toFile()

    // Collect info
    run(entry,
        sources,
        driver,
        configForProfiling(dispatchInfo),
        linkage,
        opts)((_, _, _) => ())

    assert(dispatchInfo.exists)

    // Benchmark
    benchmark(entry,
              driver,
              configForBenchmarking(dispatchInfo),
              iterations,
              sources,
              linkage,
              opts)(fn)

  }

  def withoutAndWith[T](entry: String,
                 driver: Driver,
                 iterations: Int,
                 sources: Map[String, String],
                 configFn: Config => Config = identity,
                 linkage: Map[String, String] = Map.empty,
                 opts: Seq[String] = defaultClangOptions)(fn: (BenchmarkResult, BenchmarkResult) => T): T = {
    val baseResult =
      benchmark(entry, driver, configFn, iterations, sources, linkage, opts)(identity)

    val inlineCachingResult =
      withInlineCaching(entry, driver, iterations, configFn, sources, linkage, opts)(identity)

    fn(baseResult, inlineCachingResult)
  }
}
