package scala.scalanative
package compiler
package pass

import scala.io.Source

import compiler.analysis.ClassHierarchy._
import compiler.analysis.ClassHierarchyExtractors._
import compiler.analysis.ControlFlow, ControlFlow.Block
import util.unreachable
import util.{sh, unsupported}
import nir._, Shows._, Inst.Let

/**
 * Inline caching based on information gathered at runtime.
 */
class InlineCaching(dispatchInfo: Map[String, Seq[String]], maxCandidates: Int)(implicit fresh: Fresh, top: Top) extends Pass {
  import InlineCaching._

  println("#" * 181)
  println("Dispatch info:")
  println(dispatchInfo)
  println("#" * 181)

  private def findImpl(meth: Method, clss: Class): Global =
    if (meth.in == clss) {
      assert(meth.isConcrete, s"Method ${meth.name.id} belongs to a type observed at runtime. The method should be concrete!")
      Global.Member(Global.Top(clss.name.id), meth.name.id)
    }
    else {
      clss.allmethods.filter(m => m.isConcrete && m.name.id == meth.name.id) match {
        case Seq() =>
          ???
        case Seq(m) =>
            m.in match {
              case c: Class if c.isModule =>
                val className = c.name.id.drop("module.".length)
                Global.Member(Global.Top(className), m.name.id)
              case other =>
                Global.Member(Global.Top(m.in.name.id), m.name.id)
            }
        case many =>
          many find (_.in == clss) match {
            case Some(m) =>
              Global.Member(Global.Top(clss.name.id), m.name.id)
            case None =>
              ???
          }
      }
    }


  def splitAtVirtualCall(block: Block): (Block, Let, Block) = {
    assert(block.insts exists isVirtualCall)
    block.insts span (!isVirtualCall(_)) match {
      case (l0, (x @ Let(_, Op.Method(_, _, _))) +: rest) =>
        val merge = fresh()
        val b0 = block.copy(insts = l0)
        val b1 = Block(merge, Seq(Val.Local(x.name, Type.Ptr)), rest :+ block.insts.last)
        (b0, x, b1)
      case _ =>
        unreachable
    }
  }


  private def isVirtualCall(inst: Inst): Boolean =
    inst match {
      case Let(_, Op.Method(_, _, MethodRef(_: Class, meth)))
        if meth.isVirtual =>
        true
      case _ =>
        false
    }

  private def blockToInsts(block: Block): Seq[Inst] =
    block.label +: block.insts

  private def splitBlock(block: Block): Seq[Block] =
    if (block.insts exists isVirtualCall) {
      val (b0, inst @ Let(n, Op.Method(_, obj, MethodRef(cls: Class, meth))), b1) =
        splitAtVirtualCall(block)

      val instname = s"${n.scope}.${n.id}"
      val key = s"$instname:${meth.name.id}"

      dispatchInfo get key getOrElse Seq() match {
        case candidates if 1 to maxCandidates contains candidates.length =>

          // load type String
          val tpe        = Val.Local(fresh(), cls.typeStruct)
          val typeptr    = Val.Local(fresh(), Type.Ptr)
          val typeidptr  = Val.Local(fresh(), Type.Ptr)

          val loadTpeNameInsts = Seq(
            Let(typeptr.name, Op.Load(Type.Ptr, obj)),
            Let(tpe.name, Op.Load(cls.typeStruct, typeptr)),
            Let(typeidptr.name, Op.Extract(tpe, Seq(1)))
          )

          val cmpTpeInst = (tpe: String) =>
            Let(Op.Call(compareJstringSig, compareJstring, Seq(typeidptr, Val.String(tpe), Val.String(instname))))

          val individualBlocks: Seq[(String, Local, Block)] = candidates map { c =>
            val ClassRef(clss) = Global.Top(c)
            val blockName = fresh()
            val impl = findImpl(meth, clss)
            val staticCall = Let(fresh(), Op.Copy(Val.Global(impl, Type.Ptr)))
            (c, blockName, Block(blockName, Nil, Seq(staticCall, Inst.Jump(Next.Label(b1.name, Seq(Val.Local(staticCall.name, Type.Ptr)))))))
          }

          val fallback = {
            val instName = fresh()
            val inst2 = inst.copy(name = instName)
            Block(fresh(), Nil, Seq(inst2, Inst.Jump(Next.Label(b1.name, Seq(Val.Local(instName, Type.Ptr))))))
          }

          val makeTypeComparison =
            (tpeName: String, thn: Local) => {
              val cmpTpe = cmpTpeInst(tpeName)
              (els: Local) =>
                Block(
                  name = fresh(),
                  params = Nil,
                  insts = Seq(
                    cmpTpe,
                    Inst.If(Val.Local(cmpTpe.name, Type.Bool), Next(thn), Next(els))
                  )
                )
            }

          val typeComparisons: Seq[Local => Block] = individualBlocks map { case (tpe, _, block) =>
            makeTypeComparison(tpe, block.name)
          }

          val finallyTypeComparisons: Seq[Block] =
            (typeComparisons foldRight List(fallback)) {
              case (blk, acc) => blk(acc.head.name) :: acc
            }

          val b01 = b0.copy(
            insts = b0.insts ++ loadTpeNameInsts :+ Inst.Jump(Next(finallyTypeComparisons.head.name)))

          Seq(b01) ++
            finallyTypeComparisons ++
            individualBlocks.map(_._3) ++
            Seq(fallback) ++
            splitBlock(b1)

        case _ =>
          Seq(block)
      }
    } else {
      Seq(block)
    }

  override def preDefn = {
    case define: Defn.Define =>
      val graph = ControlFlow.Graph(define.insts)
      val newBlocks = graph.all flatMap splitBlock
      Seq(define.copy(insts = newBlocks flatMap blockToInsts))
  }


}

object InlineCaching extends PassCompanion {
  def apply(ctx: Ctx) =
    ctx.options.profileInfo match {
      case Some(info) if info.exists =>
        val dispatchInfo =
          tools.DispatchInfoParser(Source.fromFile(info).mkString)
        new InlineCaching(dispatchInfo, 5)(ctx.fresh, ctx.top)

      case _ =>
        EmptyPass
    }

  val compareJstringName = Global.Top("jstring_compare")
  val compareJstringSig  = Type.Function(Seq(Arg(Rt.String), Arg(Rt.String), Arg(Rt.String)), Type.Bool)
  val compareJstring     = Val.Global(compareJstringName, compareJstringSig)

  override val injects = Seq(Defn.Declare(Attrs.None, compareJstringName, compareJstringSig))

}
