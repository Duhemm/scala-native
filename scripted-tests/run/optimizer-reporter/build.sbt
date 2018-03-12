import scalanative.build.OptimizerReporter
import scalanative.sbtplugin.ScalaNativePluginInternal.nativeOptimizerReporter

enablePlugins(ScalaNativePlugin)

scalaVersion := "2.11.12"

nativeOptimizerReporter in Compile := OptimizerReporter.toDirectory(
  crossTarget.value.toPath)

lazy val check = taskKey[Unit]("Check that dot file was created.")

check := {
  assert((crossTarget.value / "out.00.00.hnir").exists)
}
