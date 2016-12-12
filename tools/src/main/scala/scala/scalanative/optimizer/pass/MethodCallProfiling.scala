package scala.scalanative
package optimizer
package pass

import analysis.ClassHierarchy._
import analysis.ClassHierarchyExtractors._
import util.{sh, unsupported}
import nir._, Shows._, Inst.Let

class MethodCallProfiling(implicit top: Top, fresh: Fresh) extends Pass {
  import MethodCallProfiling._

  override def preDefn = {
    case defn @ Defn.Define(_, name, _, insts) =>
      val newInsts = insts.flatMap(addProfiling(name, _))
      Seq(defn.copy(insts = newInsts))
  }

  private def addProfiling(enclosingDefn: Global, inst: Inst): Seq[Inst] =
    inst match {
      case inst @ Let(n, Op.Method(packedObj, MethodRef(cls: Class, meth)))
          if meth.isVirtual =>
        val typeptr   = Val.Local(fresh(), Type.Ptr)
        val typeidptr = Val.Local(fresh(), Type.Ptr)
        val typeid    = Val.Local(fresh(), Type.I32)

        val key = {
          val enclosing = sh"$enclosingDefn".toString
          val instName  = sh"$n".toString
          val methName  = sh"${meth.name}".toString
          s"$enclosing -> $instName -> $methName"
        }
        unpacked(packedObj) { (_, obj) =>
          Seq(
            Let(typeptr.name, Op.Load(Type.Ptr, obj)),
            Let(typeidptr.name,
                Op.Elem(cls.typeStruct, typeptr, Seq(Val.I32(0), Val.I32(0)))),
            Let(typeid.name, Op.Load(Type.I32, typeidptr)),
            Let(
              Op.Call(profileMethodSig,
                      profileMethod,
                      Seq(typeid, Val.I32(key.##)))),
            inst
          )
        }

      case other =>
        Seq(other)

    }

}

object MethodCallProfiling extends PassCompanion {
  override def apply(config: tools.Config, top: Top) =
    if (config.profileDispatch)
      new MethodCallProfiling()(top, top.fresh)
    else
      EmptyPass

  val profileMethodName = Global.Top("method_call_log")
  val profileMethodSig =
    Type.Function(Seq(Arg(Type.I32), Arg(Type.I32)), Type.Void)
  val profileMethod = Val.Global(profileMethodName, profileMethodSig)

  override val injects = Seq(
    Defn.Declare(Attrs.None, profileMethodName, profileMethodSig))
}
