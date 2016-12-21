package scala.scalanative
package optimizer
package pass

import tools.Config
import analysis.ClassHierarchy.Top
import nir._

/** Replaces all occurrences of `Val.Packed` with underlying value. */
class RemovePackedVal extends Pass {

  override def preVal = PartialFunction(removePackedVal _)
  private def removePackedVal(v: Val): Val = v match {
    case Val.Struct(ty, values) =>
      Val.Struct(ty, values.map(removePackedVal))
    case Val.Array(ty, values) =>
      Val.Array(ty, values.map(removePackedVal))
    case Val.Const(value) =>
      Val.Const(removePackedVal(value))
    case Val.Packed(_, value) =>
      removePackedVal(value)
    case other =>
      other
  }
}

object RemovePackedVal extends PassCompanion {
  override def apply(config: Config, top: Top): Pass =
    new RemovePackedVal
}
