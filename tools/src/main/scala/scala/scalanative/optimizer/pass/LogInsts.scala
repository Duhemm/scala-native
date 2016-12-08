package scala.scalanative
package optimizer
package pass

import analysis.ClassHierarchy.Top
import tools.Config
import nir._
import Inst._
import Op._

class LogInsts(implicit fresh: Fresh) extends Pass {
  import LogInsts._

  private def call(ty: Type, v: Val.Global): Inst.Let =
    Let(Op.Call(ty, v, Seq()))
  override def preInst = {
    case inst: Inst.Let =>
      inst.op match {
        case c: Call =>
          Seq(call(log_callSig, log_call), inst)

        case l: Load =>
          Seq(call(log_loadSig, log_load), inst)

        case s: Store =>
          Seq(call(log_storeSig, log_store), inst)

        case e: Elem =>
          Seq(call(log_elemSig, log_elem), inst)

        case e: Extract =>
          Seq(call(log_extractSig, log_extract), inst)

        case i: Insert =>
          Seq(call(log_insertSig, log_insert), inst)

        case s: Stackalloc =>
          Seq(call(log_stackallocSig, log_stackalloc), inst)

        case b: Op.Bin =>
          Seq(call(log_binSig, log_bin), inst)

        case c: Op.Comp =>
          Seq(call(log_compSig, log_comp), inst)

        case c: Op.Conv =>
          Seq(call(log_convSig, log_conv), inst)

        case s: Select =>
          Seq(call(log_selectSig, log_select), inst)

        case _ =>
          Seq(inst)
      }
    case other =>
      Seq(other)
  }
}

object LogInsts extends PassCompanion {

  val log_callSig  = Type.Function(Seq(), Type.Void)
  val log_call     = Val.Global(Global.Top("log_call"), Type.Ptr)
  val log_callDecl = Defn.Declare(Attrs.None, log_call.name, log_callSig)

  val log_loadSig  = Type.Function(Seq(), Type.Void)
  val log_load     = Val.Global(Global.Top("log_load"), Type.Ptr)
  val log_loadDecl = Defn.Declare(Attrs.None, log_load.name, log_loadSig)

  val log_storeSig  = Type.Function(Seq(), Type.Void)
  val log_store     = Val.Global(Global.Top("log_store"), Type.Ptr)
  val log_storeDecl = Defn.Declare(Attrs.None, log_store.name, log_storeSig)

  val log_elemSig  = Type.Function(Seq(), Type.Void)
  val log_elem     = Val.Global(Global.Top("log_elem"), Type.Ptr)
  val log_elemDecl = Defn.Declare(Attrs.None, log_elem.name, log_elemSig)

  val log_extractSig = Type.Function(Seq(), Type.Void)
  val log_extract    = Val.Global(Global.Top("log_extract"), Type.Ptr)
  val log_extractDecl =
    Defn.Declare(Attrs.None, log_extract.name, log_extractSig)

  val log_insertSig  = Type.Function(Seq(), Type.Void)
  val log_insert     = Val.Global(Global.Top("log_insert"), Type.Ptr)
  val log_insertDecl = Defn.Declare(Attrs.None, log_insert.name, log_insertSig)

  val log_stackallocSig = Type.Function(Seq(), Type.Void)
  val log_stackalloc    = Val.Global(Global.Top("log_stackalloc"), Type.Ptr)
  val log_stackallocDecl =
    Defn.Declare(Attrs.None, log_stackalloc.name, log_stackallocSig)

  val log_binSig = Type.Function(Seq(), Type.Void)
  val log_bin    = Val.Global(Global.Top("log_bin"), Type.Ptr)
  val log_binDecl =
    Defn.Declare(Attrs.None, log_bin.name, log_binSig)

  val log_compSig = Type.Function(Seq(), Type.Void)
  val log_comp    = Val.Global(Global.Top("log_comp"), Type.Ptr)
  val log_compDecl =
    Defn.Declare(Attrs.None, log_comp.name, log_compSig)

  val log_convSig = Type.Function(Seq(), Type.Void)
  val log_conv    = Val.Global(Global.Top("log_conv"), Type.Ptr)
  val log_convDecl =
    Defn.Declare(Attrs.None, log_conv.name, log_convSig)

  val log_selectSig  = Type.Function(Seq(), Type.Void)
  val log_select     = Val.Global(Global.Top("log_select"), Type.Ptr)
  val log_selectDecl = Defn.Declare(Attrs.None, log_select.name, log_selectSig)

  override def injects: Seq[Defn] =
    Seq(log_callDecl,
        log_loadDecl,
        log_storeDecl,
        log_elemDecl,
        log_extractDecl,
        log_insertDecl,
        log_stackallocDecl,
        log_binDecl,
        log_compDecl,
        log_convDecl,
        log_selectDecl)

  override def apply(config: Config, top: Top): Pass =
    new LogInsts()(top.fresh)
}
