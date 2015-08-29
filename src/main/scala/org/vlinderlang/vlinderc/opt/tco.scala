package org.vlinderlang.vlinderc.opt

import org.vlinderlang.vlinderc.ssa._
import scala.collection.immutable.ListMap

object TCO extends Optimization {
  override def name = "Tail call optimization"

  override def breaking = false

  override def apply(cfg: CFG): CFG =
    cfg.copy(blocks = cfg.blocks.par.mapValues(optimizeBlock).seq.toMap)

  private def optimizeBlock(block: Block): Block =
    block.copy(insts = ListMap(optimizeInsts(block.insts.toList): _*))

  private def optimizeInsts(insts: List[(InstID, Inst)]): List[(InstID, Inst)] =
    insts.reverse match {
      case (_, RetInst(retvalID)) +: (callID, call: CallInst) +: xs if callID == retvalID =>
        ((callID, call.copy(tailcall = true)) +: xs).reverse
      case _ => insts
    }
}
