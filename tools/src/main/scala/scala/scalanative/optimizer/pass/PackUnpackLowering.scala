package scala.scalanative
package optimizer
package pass

import scala.collection.mutable
import analysis.ClassHierarchy.Top
import nir._
import tools.Config

/** Lowers `Val.Packed`, `Op.Pack`, `Op.UnpackId`, `Op.UnpackPtr` */
class PackUnpackLowering(implicit fresh: Fresh) extends Pass {

  override def preInst = PartialFunction(lowerPacked _)
  override def postInst = {

    case Inst.Let(n, Op.Pack(value, ptr)) =>
      val asLongPtr   = Val.Local(fresh(), Type.I64)
      val asLongValue = Val.Local(fresh(), Type.I64)
      val shifted     = Val.Local(fresh(), Type.I64)
      val pckd        = Val.Local(fresh(), Type.I64)

      Seq(
        Inst.Let(asLongPtr.name, Op.Conv(Conv.Ptrtoint, Type.I64, ptr)),
        Inst.Let(asLongValue.name, Op.Conv(Conv.Zext, Type.I64, value)),
        Inst.Let(shifted.name,
                 Op.Bin(Bin.Shl, Type.I64, asLongValue, Val.I64(48L))),
        Inst.Let(pckd.name, Op.Bin(Bin.Or, Type.I64, shifted, asLongPtr)),
        Inst.Let(n, Op.Conv(Conv.Inttoptr, Type.Ptr, pckd))
      )

    case Inst.Let(n, Op.UnpackId(v: Val.Packed)) =>
      Seq(
        Inst.Let(n, Op.Copy(Val.I16(v.id)))
      )

    case Inst.Let(n, Op.UnpackId(packedPtr)) =>
      val asLong  = Val.Local(fresh(), Type.I64)
      val value   = Val.Local(fresh(), Type.I64)
      val shifted = Val.Local(fresh(), Type.I64)
      Seq(
        Inst.Let(asLong.name, Op.Conv(Conv.Ptrtoint, Type.I64, packedPtr)),
        Inst.Let(value.name,
                 Op.Bin(Bin.And, Type.I64, asLong, Val.I64(VALUE_MASK))),
        Inst.Let(shifted.name,
                 Op.Bin(Bin.Lshr, Type.I64, value, Val.I64(48L))),
        Inst.Let(n, Op.Conv(Conv.Trunc, Type.I16, shifted))
      )

    case Inst.Let(n, Op.UnpackPtr(v: Val.Packed)) =>
      Seq(
        Inst.Let(n, Op.Copy(v.value))
      )

    case Inst.Let(n, Op.UnpackPtr(packedPtr)) =>
      val asLong   = Val.Local(fresh(), Type.I64)
      val unpacked = Val.Local(fresh(), Type.I64)
      Seq(
        Inst.Let(asLong.name, Op.Conv(Conv.Ptrtoint, Type.I64, packedPtr)),
        Inst.Let(unpacked.name,
                 Op.Bin(Bin.And, Type.I64, asLong, Val.I64(PTR_MASK))),
        Inst.Let(n, Op.Conv(Conv.Inttoptr, Type.Ptr, unpacked))
      )
  }

  private val VALUE_MASK = 0xFFFF000000000000L
  private val PTR_MASK   = 0x0000FFFFFFFFFFFFL

  /**
   * Lowers `Val.Packed` using `Op.Pack`.
   *
   * @param v The value to lower
   * @return A pair whose first element contains the instructions to perform
   *         the lowering. The second element is the new lowered value.
   */
  private def lowerPacked(v: Val): (Seq[Inst], Val) = v match {

    case Val.Struct(ty, values) =>
      val preamble = mutable.UnrolledBuffer.empty[Inst]
      val newValues = values.map { v =>
        val (newInsts, newValue) = lowerPacked(v)
        preamble ++= newInsts
        newValue
      }
      (preamble, Val.Struct(ty, newValues))

    case Val.Array(ty, values) =>
      val preamble = mutable.UnrolledBuffer.empty[Inst]
      val newValues = values.map { v =>
        val (newInsts, newValue) = lowerPacked(v)
        preamble ++= newInsts
        newValue
      }
      (preamble, Val.Array(ty, newValues))

    case Val.Const(value) =>
      val (insts, newValue) = lowerPacked(value)
      (insts, Val.Const(newValue))

    case Val.Packed(id, value) =>
      val (preamble, v0) = lowerPacked(value)
      val newVal         = Val.Local(fresh(), v0.ty)
      val insts = Seq(
        Inst.Let(newVal.name, Op.Pack(Val.I16(id), v0))
      )
      (insts, newVal)

    case other =>
      (Seq.empty, other)
  }

  private def lowerPacked(next: Next): (Seq[Inst], Next) = next match {
    case Next.Case(value, name) =>
      val (insts, newValue) = lowerPacked(value)
      (insts, Next.Case(newValue, name))
    case Next.Label(name, args) =>
      val (insts, newArgs) = args.map(lowerPacked).unzip
      (insts.flatten, Next.Label(name, newArgs))
    case other =>
      (Seq.empty, other)
  }

  private def lowerPacked(inst: Inst): Seq[Inst] = inst match {
    case Inst.None =>
      Seq(Inst.None)
    case label: Inst.Label =>
      Seq(label)

    case Inst.Let(n, Op.Call(ty, ptr, args)) =>
      val (ptrInsts, newPtr)   = lowerPacked(ptr)
      val (argsInsts, newArgs) = args.map(lowerPacked).unzip
      ptrInsts ++ argsInsts.flatten :+ Inst.Let(n,
                                                Op.Call(ty, newPtr, newArgs))

    case Inst.Let(n, Op.Load(ty, ptr)) =>
      val (insts, newPtr) = lowerPacked(ptr)
      insts :+ Inst.Let(n, Op.Load(ty, newPtr))

    case Inst.Let(n, Op.Store(ty, ptr, value)) =>
      val (ptrInsts, newPtr)     = lowerPacked(ptr)
      val (valueInsts, newValue) = lowerPacked(value)
      ptrInsts ++ valueInsts :+ Inst.Let(n, Op.Store(ty, newPtr, newValue))

    case Inst.Let(n, Op.Elem(ty, ptr, indices)) =>
      val (ptrInsts, newPtr)         = lowerPacked(ptr)
      val (indicesInsts, newIndices) = indices.map(lowerPacked).unzip
      ptrInsts ++ indicesInsts.flatten :+ Inst
        .Let(n, Op.Elem(ty, newPtr, newIndices))

    case Inst.Let(n, Op.Extract(aggr, indices)) =>
      val (insts, newAggr) = lowerPacked(aggr)
      insts :+ Inst.Let(n, Op.Extract(newAggr, indices))

    case Inst.Let(n, Op.Insert(aggr, value, indices)) =>
      val (aggrInsts, newAggr)   = lowerPacked(aggr)
      val (valueInsts, newValue) = lowerPacked(value)
      aggrInsts ++ valueInsts :+ Inst
        .Let(n, Op.Insert(newAggr, newValue, indices))

    case Inst.Let(n, Op.Stackalloc(ty, value)) =>
      val (insts, newValue) = lowerPacked(value)
      insts :+ Inst.Let(n, Op.Stackalloc(ty, newValue))

    case Inst.Let(n, Op.Bin(bin, ty, l, r)) =>
      val (lInsts, newL) = lowerPacked(l)
      val (rInsts, newR) = lowerPacked(r)
      lInsts ++ rInsts :+ Inst.Let(n, Op.Bin(bin, ty, newL, newR))

    case Inst.Let(n, Op.Comp(comp, ty, l, r)) =>
      val (lInsts, newL) = lowerPacked(l)
      val (rInsts, newR) = lowerPacked(r)
      lInsts ++ rInsts :+ Inst.Let(n, Op.Comp(comp, ty, newL, newR))

    case Inst.Let(n, Op.Conv(conv, ty, v)) =>
      val (insts, newValue) = lowerPacked(v)
      insts :+ Inst.Let(n, Op.Conv(conv, ty, newValue))

    case Inst.Let(n, Op.Select(cond, thenv, elsev)) =>
      val (condInsts, newCond)   = lowerPacked(cond)
      val (thenvInsts, newThenv) = lowerPacked(thenv)
      val (elsevInsts, newElsev) = lowerPacked(elsev)
      condInsts ++ thenvInsts ++ elsevInsts :+ Inst
        .Let(n, Op.Select(newCond, newThenv, newElsev))

    case Inst.Ret(v) =>
      val (insts, newValue) = lowerPacked(v)
      insts :+ Inst.Ret(newValue)

    case Inst.Jump(next) =>
      val (insts, newNext) = lowerPacked(next)
      insts :+ Inst.Jump(newNext)

    case Inst.If(value, thenp, elsep) =>
      val (valueInsts, newValue) = lowerPacked(value)
      val (thenpInsts, newThenp) = lowerPacked(thenp)
      val (elsepInsts, newElsep) = lowerPacked(elsep)
      valueInsts ++ thenpInsts ++ elsepInsts :+ Inst
        .If(newValue, newThenp, newElsep)

    case Inst.Switch(value, default, cases) =>
      val (valueInsts, newValue)     = lowerPacked(value)
      val (defaultInsts, newDefault) = lowerPacked(default)
      val (casesInsts, newCases)     = cases.map(lowerPacked).unzip
      valueInsts ++ defaultInsts ++ casesInsts.flatten :+ Inst
        .Switch(newValue, newDefault, newCases)

    case Inst.Invoke(ty, ptr, args, succ, fail) =>
      val (ptrInsts, newPtr)   = lowerPacked(ptr)
      val (argsInsts, newArgs) = args.map(lowerPacked).unzip
      val (succInsts, newSucc) = lowerPacked(succ)
      val (failInsts, newFail) = lowerPacked(fail)
      ptrInsts ++ argsInsts.flatten ++ succInsts ++ failInsts :+ Inst
        .Invoke(ty, newPtr, newArgs, newSucc, newFail)

    case Inst.Throw(value) =>
      val (insts, newValue) = lowerPacked(value)
      insts :+ Inst.Throw(newValue)

    case Inst.Try(succ, fail) =>
      val (succInsts, newSucc) = lowerPacked(succ)
      val (failInsts, newFail) = lowerPacked(fail)
      succInsts ++ failInsts :+ Inst.Try(newSucc, newFail)

    case other =>
      Seq(other)
  }

}

object PackUnpackLowering extends PassCompanion {
  override def apply(config: tools.Config, top: Top): Pass =
    new PackUnpackLowering()(top.fresh)

}
