package scala.scalanative
package compiler
package pass

import scala.io.Source

import compiler.analysis.ClassHierarchy._
import compiler.analysis.ClassHierarchyExtractors._
import util.{sh, unsupported}
import nir._, Shows._, Inst.Let

/**
 * Inline monomorphic call sites
 */
class MonomorphicInlining(dispatchInfo: Map[String, Seq[String]])(implicit top: Top) extends Pass {
  import MonomorphicInlining._

  private def findImpl(meth: Method, clss: Class): String =
    if (meth.in == clss) {
      assert(meth.isConcrete, s"Method ${meth.name.id} belongs to a type observed at runtime. The method should be concrete!")
      s"${clss.name.id}::${meth.name.id}"
    }
    else {
      clss.allmethods.filter(m => m.isConcrete && m.name.id == meth.name.id) match {
        case Seq() =>
          ???
        case Seq(m) =>
            m.in match {
              case c: Class if c.isModule =>
                val className = c.name.id.drop("module.".length)
                s"$className::${m.name.id}"
              case other =>
                s"${m.in.name.id}::${m.name.id}"
            }

        case many =>
          many find (_.in == clss) match {
            case Some(m) =>
              s"${clss.name.id}::${m.name.id}"
            case None =>
              ???
          }
      }
    }

  override def preInst = {
    case inst @ Let(n, Op.Method(_, _, MethodRef(_: Class, meth)))
        if meth.isVirtual =>

      val instname = s"${n.scope}.${n.id}"
      val key = s"$instname:${meth.name.id}"

      dispatchInfo get key getOrElse Seq() match {

        case Seq(mono) =>
          val ClassRef(clss) = Global.Top(mono)
          val implName = findImpl(meth, clss)
          Seq(Let(n, Op.Copy(Val.Global(Global.Top(implName), Type.Ptr))))

        case _ =>
          Seq(inst)
      }
  }
}

object MonomorphicInlining extends PassCompanion {
  def apply(ctx: Ctx) =
    ctx.options.profileInfo match {
      case Some(info) if info.exists =>
        val dispatchInfo =
          tools.DispatchInfoParser(Source.fromFile(info).mkString)
        new MonomorphicInlining(dispatchInfo)(ctx.top)

      case _ =>
        EmptyPass
    }
}
