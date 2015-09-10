package org.vlinderlang.vlinderc

package object ssa2bc {
  sealed abstract class Inst
  case class CallInst(VLINDERarguments: Int) extends Inst
  case class LdargInst(VLINDERindex: Int) extends Inst
  case class LdlocInst(VLINDERindex: Int) extends Inst
  case class LdgblInst(VLINDERname: String) extends Inst
  case class LdstrInst(VLINDERvalue: String) extends Inst
  case class NewInst(VLINDERtype: String) extends Inst
  case class StlocInst(VLINDERindex: Int) extends Inst
  case class TailcallInst(VLINDERarguments: Int) extends Inst
  case object RetInst extends Inst
  case class StfldInst(VLINDERfield: String) extends Inst

  def convert(cfg: ssa.CFG): Vector[Inst] = {
    var insts = Vector[Inst]()

    var locals = Map[ssa.InstID, Int]()
    var lastLocalIndex = -1
    def newLocal(instID: ssa.InstID): Int = {
      lastLocalIndex += 1
      locals += (instID -> lastLocalIndex)
      lastLocalIndex
    }

    cfg.blocks(cfg.entry).insts.toVector foreach {
      case (id, ssa.CallInst(callee, arguments, tailcall)) =>
        insts :+= LdlocInst(locals(callee))
        for (argument <- arguments) {
          insts :+= LdlocInst(locals(argument))
        }
        insts :+= (if (tailcall) TailcallInst else CallInst)(arguments.size)
        insts :+= StlocInst(newLocal(id))

      case (id, ssa.LdargInst(index)) =>
        insts :+= LdargInst(index)
        insts :+= StlocInst(newLocal(id))

      case (id, ssa.LdgblInst(module, member)) =>
        insts :+= LdgblInst(module.segments.mkString(".") + "." + member)
        insts :+= StlocInst(newLocal(id))

      case (id, ssa.LdstrInst(value)) =>
        insts :+= LdstrInst(value)
        insts :+= StlocInst(newLocal(id))

      case (id, ssa.NewInst(type_)) =>
        insts :+= NewInst(type_.descriptor)
        insts :+= StlocInst(newLocal(id))

      case (_, ssa.RetInst(value)) =>
        insts :+= LdlocInst(locals(value))
        insts :+= RetInst

      case (id, ssa.StfldInst(target, field, value)) =>
        insts :+= LdlocInst(locals(target))
        insts :+= LdlocInst(locals(value))
        insts :+= StfldInst(field)
        insts :+= StlocInst(newLocal(id))
    }

    insts
  }
}
