package scala.scalanative
package optimizer
package pass

import analysis.ClassHierarchy._
import analysis.ClassHierarchyExtractors._
import nir._, Inst.Let

/** Translates instance checks to range checks on type ids. */
class IsLowering(implicit fresh: Fresh, top: Top) extends Pass {

  override def preInst = eliminateIs

  private def eliminateIs: PartialFunction[Inst, Seq[Inst]] = {
    case Let(n, Op.Is(_, Val.Zero(_))) =>
      Seq(Let(n, Op.Copy(Val.False)))

    case isInst @ Let(n, Op.Is(ty, packedObj)) =>
      val isNullV = Val.Local(fresh(), Type.Bool)
      val isIsV   = Val.Local(fresh(), Type.Bool)

      val thenL = fresh()
      val elseL = fresh()
      val contL = fresh()

      val thenResultV = Val.Local(fresh(), Type.Bool)
      val elseResultV = Val.Local(fresh(), Type.Bool)
      val obj         = Val.Local(fresh(), Type.Ptr)
      val id          = Val.Local(fresh(), Type.I16)
      Seq(
        Let(obj.name, Op.UnpackPtr(packedObj)),
        Let(id.name, Op.UnpackId(packedObj)),
        // if
        Let(isNullV.name,
            Op.Comp(Comp.Ieq, Type.Ptr, obj, Val.Zero(Type.Ptr))),
        Inst.If(isNullV, Next(thenL), Next(elseL)),
        // then
        Inst.Label(thenL, Seq.empty),
        Let(thenResultV.name, Op.Copy(Val.False)),
        Inst.Jump(Next.Label(contL, Seq(thenResultV))),
        // else
        Inst.Label(elseL, Seq.empty)
      ) ++
        doEliminateIs(Let(elseResultV.name, Op.Is(ty, obj)), id) ++
        Seq(Inst.Jump(Next.Label(contL, Seq(elseResultV))),
            // cont
            Inst.Label(contL, Seq(isIsV)),
            Let(n, Op.Copy(isIsV)))
    case other => Seq(other)
  }

  private def doEliminateIs(inst: Inst.Let, unpackedId: Val): Seq[Inst] =
    inst match {
      case Let(n, Op.Is(ClassRef(cls), obj)) if cls.range.length == 1 =>
        Seq(
          Let(n,
              Op.Comp(Comp.Ieq, Type.I16, unpackedId, Val.I16(cls.id.toShort)))
        )

      case Let(n, Op.Is(ClassRef(cls), obj)) =>
        val ge = Val.Local(fresh(), Type.Bool)
        val le = Val.Local(fresh(), Type.Bool)

        Seq(
          Let(ge.name,
              Op.Comp(Comp.Sle,
                      Type.I16,
                      Val.I16(cls.range.start.toShort),
                      unpackedId)),
          Let(le.name,
              Op.Comp(Comp.Sle,
                      Type.I16,
                      unpackedId,
                      Val.I16(cls.range.end.toShort))),
          Let(n, Op.Bin(Bin.And, Type.Bool, ge, le))
        )

      case Let(n, Op.Is(TraitRef(trt), obj)) =>
        val boolptr = Val.Local(fresh(), Type.Ptr)

        Seq(
          Let(boolptr.name,
              Op.Elem(top.instanceTy,
                      top.instanceVal,
                      Seq(Val.I32(0), unpackedId, Val.I32(trt.id)))),
          Let(n, Op.Load(Type.Bool, boolptr))
        )
      case other => Seq(other)
    }

}

object IsLowering extends PassCompanion {
  override def apply(config: tools.Config, top: Top) =
    new IsLowering()(top.fresh, top)
}
