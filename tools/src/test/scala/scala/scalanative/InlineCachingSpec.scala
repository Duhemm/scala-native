package scala.scalanative

import java.io.File
import java.nio.file.Files

import optimizer.Driver
import tools.Config

abstract class InlineCachingSpec extends BenchmarkSpec {

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
      (out: File) => configFn andThen (_.withProfileDispatchInfo(Some(out)))

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
