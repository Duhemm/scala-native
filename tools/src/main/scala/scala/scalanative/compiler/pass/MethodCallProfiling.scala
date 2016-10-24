package scala.scalanative
package compiler
package pass

import compiler.analysis.ClassHierarchy._
import compiler.analysis.ClassHierarchyExtractors._
import util.{sh, unsupported}
import nir._, Shows._, Inst.Let

class MethodCallProfiling(implicit top: Top, fresh: Fresh) extends Pass {
  import MethodCallProfiling._

  override def preInst = {
    case inst @ Let(n, Op.Method(sig, obj, MethodRef(cls: Class, meth)))
        if meth.isVirtual =>
      val typeptr   = Val.Local(fresh(), Type.Ptr)
      val typeidptr = Val.Local(fresh(), Type.Ptr)
      val typeid    = Val.Local(fresh(), Type.I32)

      val instname = s"${n.scope}.${n.id}"

      Seq(
          Let(typeptr.name, Op.Load(Type.Ptr, obj)),
          Let(typeidptr.name,
               Op.Elem(cls.typeStruct, typeptr, Seq(Val.I32(0), Val.I32(0)))),
          Let(typeid.name, Op.Load(Type.I32, typeidptr)),
          Let(Op.Call(callCountSig, callCount, Seq())),
          Let(
              Op.Call(profileMethodSig,
                      profileMethod,
                      Seq(typeid, Val.String(s"$instname:${meth.name.id}")))),
          inst
      )
  }
}

object MethodCallProfiling extends PassCompanion {
  def apply(ctx: Ctx) =
    if (ctx.options.profileMethodCalls)
      new MethodCallProfiling()(ctx.top, ctx.fresh)
    else
      EmptyPass

  val callCountName = Global.Top("method_call_count")
  val callCountSig  = Type.Function(Seq(), Type.Void)
  val callCount     = Val.Global(callCountName, callCountSig)

  val profileMethodName = Global.Top("method_call_log")
  val profileMethodSig  = Type.Function(Seq(Arg(Type.I32), Arg(Rt.String)), Type.Void)
  val profileMethod     = Val.Global(profileMethodName, profileMethodSig)

  override val injects = Seq(
      Defn.Declare(Attrs.None, profileMethodName, profileMethodSig),
      Defn.Declare(Attrs.None, callCountName, callCountSig))
}
