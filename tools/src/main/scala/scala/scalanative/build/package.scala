package scala.scalanative

import java.nio.file.{Files, Path, Paths}
import java.util.Arrays

import scala.sys.process.Process

import build.IO.RichPath
import nir.Global

package object build {

  type LinkerPath = linker.ClassPath
  val LinkerPath = linker.ClassPath

  type LinkerReporter = linker.Reporter
  val LinkerReporter = linker.Reporter

  type LinkerResult = linker.Result
  val LinkerResult = linker.Result

  type OptimizerDriver = optimizer.Driver
  val OptimizerDriver = optimizer.Driver

  type OptimizerReporter = optimizer.Reporter
  val OptimizerReporter = optimizer.Reporter

  /** Given the classpath and main entry point, link under closed-world
   *  assumption.
   */
  def link(config: Config): LinkerResult = {
    val result = config.logger.time("Linking") {
      val chaDeps   = optimizer.analysis.ClassHierarchy.depends
      val passes    = config.driver.passes
      val passDeps  = passes.flatMap(_.depends).distinct
      val deps      = (chaDeps ++ passDeps).distinct
      val injects   = passes.flatMap(_.injects)
      val mainClass = nir.Global.Top(config.entry)
      val entry =
        nir.Global
          .Member(mainClass, "main_scala.scalanative.runtime.ObjectArray_unit")
      val result =
        (linker.Linker(config)).link(entry +: deps)

      result.withDefns(result.defns ++ injects)
    }

    if (result.unresolved.nonEmpty) {
      result.unresolved.map(_.show).sorted.foreach { signature =>
        config.logger.error(s"cannot link: $signature")
      }
      throw new Exception("unable to link")
    }
    val classCount = result.defns.count {
      case _: nir.Defn.Class | _: nir.Defn.Module | _: nir.Defn.Trait => true
      case _                                                          => false
    }
    val methodCount = result.defns.count(_.isInstanceOf[nir.Defn.Define])
    config.logger.info(
      s"Discovered ${classCount} classes and ${methodCount} methods")

    result
  }

  /** Link just the given entries, disregarding the extra ones that are
   *  needed for the optimizer and/or codegen.
   */
  def linkRaw(config: Config, entries: Seq[nir.Global]): LinkerResult =
    config.logger.time("Linking") {
      linker.Linker(config).link(entries)
    }

  /** Transform high-level closed world to its lower-level counterpart. */
  def optimize(config: Config,
               assembly: Seq[nir.Defn],
               dyns: Seq[String]): Seq[nir.Defn] =
    config.logger.time(s"Optimizing (${config.driver.mode} mode)") {
      optimizer.Optimizer(config, assembly, dyns)
    }

  /** Given low-level assembly, emit LLVM IR for it to the buildDirectory. */
  def codegen(config: Config, assembly: Seq[nir.Defn]): Unit = {
    config.logger.time("Generating intermediate code") {
      scalanative.codegen.CodeGen(config, assembly)
    }
    val produced = IO.getAll(config.workdir, "glob:**.ll")
    config.logger.info(s"Produced ${produced.length} files")
  }

  def build(nativeLib: Path,
            paths: Seq[Path],
            entry: String,
            target: Path,
            workdir: Path,
            logger: Logger): Path = {
    val config = Config.default(nativeLib, paths, entry, workdir, logger)
    build(config, target)
  }

  def build(config: Config, target: Path) = {
    val linkerResult = link(config)
    val optimized =
      optimize(config, linkerResult.defns, linkerResult.dyns)
    val generated = {
      codegen(config, optimized)
      IO.getAll(config.workdir, "glob:**.ll")
    }
    val objectFiles = LLVM.compileLL(config, generated)
    val unpackedLib = unpackNativeLibrary(config.nativeLib, config.workdir)

    val nativeLibConfig =
      config.withCompileOptions("-O2" +: config.compileOptions)
    val _ =
      compileNativeLib(nativeLibConfig, linkerResult, unpackedLib)

    LLVM.linkLL(config, linkerResult, objectFiles, unpackedLib, target)
  }

  /**
   * Unpack the `nativelib` to `workdir/lib`.
   *
   * If the same archive has already been unpacked to this location, this
   * call has no effects.
   *
   * @param nativelib The JAR to unpack.
   * @param workdir   The working directory. The nativelib will be unpacked
   *                  to `workdir/lib`.
   * @return The location where the nativelib has been unpacked, `workdir/lib`.
   */
  def unpackNativeLibrary(nativeLib: Path, workdir: Path): Path = {
    val lib         = workdir.resolve("lib")
    val jarhash     = IO.sha1(nativeLib)
    val jarhashPath = lib.resolve("jarhash")
    def unpacked =
      Files.exists(lib) &&
        Files.exists(jarhashPath) &&
        Arrays.equals(jarhash, Files.readAllBytes(jarhashPath))

    if (!unpacked) {
      IO.deleteRecursive(lib)
      IO.unzip(nativeLib, lib)
      IO.write(jarhashPath, jarhash)
    }

    lib
  }

  def compileNativeLib(config: Config,
                       linkerResult: LinkerResult,
                       libPath: Path): Path = {
    val cpaths   = IO.getAll(config.workdir, "glob:**.c").map(_.abs)
    val cpppaths = IO.getAll(config.workdir, "glob:**.cpp").map(_.abs)
    val paths    = cpaths ++ cpppaths

    // predicate to check if given file path shall be compiled
    // we only include sources of the current gc and exclude
    // all optional dependencies if they are not necessary
    val optPath = libPath.resolve("optional").abs
    val (gcPath, gcSelPath) = {
      val gcPath    = libPath.resolve("gc")
      val gcSelPath = gcPath.resolve(config.gc.name)
      (gcPath.abs, gcSelPath.abs)
    }

    def include(path: String) = {
      if (path.contains(optPath)) {
        val name = Paths.get(path).toFile.getName.split("\\.").head
        linkerResult.links.map(_.name).contains(name)
      } else if (path.contains(gcPath)) {
        path.contains(gcSelPath)
      } else {
        true
      }
    }

    // delete .o files for all excluded source files
    paths.foreach { path =>
      if (!include(path)) {
        val ofile = Paths.get(path + ".o")
        if (Files.exists(ofile)) {
          Files.delete(ofile)
        }
      }
    }

    // generate .o files for all included source files in parallel
    paths.par.foreach { path =>
      val opath = path + ".o"
      if (include(path) && !Files.exists(Paths.get(opath))) {
        val isCpp    = path.endsWith(".cpp")
        val compiler = if (isCpp) config.clangpp.abs else config.clang.abs
        val flags    = (if (isCpp) Seq("-std=c++11") else Seq()) ++ config.compileOptions
        val compilec = Seq(compiler) ++ flags ++ Seq("-c", path, "-o", opath)

        config.logger.running(compilec)
        val result = Process(compilec, config.workdir.toFile) ! Logger
          .toProcessLogger(config.logger)
        if (result != 0) {
          sys.error("Failed to compile native library runtime code.")
        }
      }
    }

    libPath
  }

}
