package org.vlinderlang.vlinderc

package object ssa2bc {
  import com.google.gson.{Gson, JsonElement}

  sealed abstract class Inst
  case class CallInst(arguments: Int) extends Inst
  case class LdargInst(index: Int) extends Inst
  case class LdlocInst(index: Int) extends Inst
  case class LdgblInst(name: String) extends Inst
  case class LdstrInst(value: String) extends Inst
  case class NewInst(`type`: String) extends Inst
  case class StlocInst(index: Int) extends Inst
  case class TailcallInst(arguments: Int) extends Inst
  case object RetInst extends Inst
  case class StfldInst(field: String) extends Inst

  def convertCFG(cfg: ssa.CFG): Vector[Inst] = {
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

      case (id, ssa.RetInst(value)) =>
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
