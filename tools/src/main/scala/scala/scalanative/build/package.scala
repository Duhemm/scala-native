package scala.scalanative

import java.nio.file.{Files, Path, Paths}
import java.util.Arrays

import scala.sys.process.Process

import tools.{
  Config,
  GarbageCollector,
  IO,
  LinkerReporter,
  LinkerResult,
  Logger,
  OptimizerDriver
}
import IO.RichPath

package object build {

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
      Files.write(jarhashPath, jarhash)
    }

    lib
  }

  def compileNativeLib(linkerResult: LinkerResult,
                       workdir: Path,
                       clang: Path,
                       clangpp: Path,
                       compileOptions: Seq[String],
                       nativelib: Path,
                       gc: GarbageCollector,
                       crossTarget: Path,
                       logger: Logger): Path = {
    val cpaths   = IO.getAll(workdir, "glob:*.c").map(_.abs)
    val cpppaths = IO.getAll(workdir, "glob:*.cpp").map(_.abs)
    val paths    = cpaths ++ cpppaths

    // predicate to check if given file path shall be compiled
    // we only include sources of the current gc and exclude
    // all optional dependencies if they are not necessary
    val libPath = crossTarget.resolve("native").resolve("lib")
    val optPath = libPath.resolve("optional").abs
    val (gcPath, gcSelPath) = {
      val gcPath    = libPath.resolve("gc")
      val gcSelPath = gcPath.resolve(gc.name)
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
        val compiler = if (isCpp) clangpp.abs else clang.abs
        val flags    = (if (isCpp) Seq("-std=c++11") else Seq()) ++ compileOptions
        val compilec = Seq(compiler) ++ flags ++ Seq("-c", path, "-o", opath)

        logger.running(compilec)
        val result = Process(compilec, workdir.toFile) ! Logger.toProcessLogger(
          logger)
        if (result != 0) {
          sys.error("Failed to compile native library runtime code.")
        }
      }
    }

    nativelib
  }

  /** Links the NIR files on classpath, reports linking errors. */
  def linkNIR(driver: OptimizerDriver,
              config: Config,
              reporter: LinkerReporter,
              logger: Logger): LinkerResult = {
    val result = logger.time("Linking") {
      tools.link(config, driver, reporter)
    }
    if (result.unresolved.nonEmpty) {
      result.unresolved.map(_.show).sorted.foreach { signature =>
        logger.error(s"cannot link: $signature")
      }
      throw new Exception("unable to link")
    }
    val classCount = result.defns.count {
      case _: nir.Defn.Class | _: nir.Defn.Module | _: nir.Defn.Trait => true
      case _                                                          => false
    }
    val methodCount = result.defns.count(_.isInstanceOf[nir.Defn.Define])
    logger.info(s"Discovered ${classCount} classes and ${methodCount} methods")
    result
  }

}