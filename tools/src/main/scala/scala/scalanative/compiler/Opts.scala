package scala.scalanative
package compiler

import java.io.File

final case class Opts(classpath: Seq[String],
                      outpath: String,
                      dotpath: Option[String],
                      entry: String,
                      verbose: Boolean,
                      sharedLibrary: Boolean,
                      profileMethodCalls: Boolean,
                      profileInfo: Option[File],
                      inlineCachingMaxCandidates: Int)
