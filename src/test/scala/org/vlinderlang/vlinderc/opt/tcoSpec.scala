package org.vlinderlang.vlinderc.opt

import org.scalatest.FlatSpec
import org.vlinderlang.vlinderc.ModuleName
import org.vlinderlang.vlinderc.ssa._
import org.vlinderlang.vlinderc.`type`.TupleType

class tcoSpec extends FlatSpec {
  "TCO" should "do nothing if there are no tail calls" in {
    val cfg = {
      val b = new CFGBuilder
      val resultID = b.inst(NewInst(TupleType()))
      b.inst(RetInst(resultID))
      b.result
    }
    assert(TCO(cfg) == cfg)
  }

  it should "perform tail call optimization" in {
    val cfg = {
      val b = new CFGBuilder
      val calleeID = b.inst(LdgblInst(ModuleName("main"), "main"))
      val argumentIDs = Vector()
      val callID = b.inst(CallInst(calleeID, argumentIDs, tailcall = false))
      b.inst(RetInst(callID))
      b.result
    }
    val tcod = TCO(cfg)
    assert(tcod.blocks(tcod.entry).insts.size == 2)
    assert(tcod.blocks(tcod.entry).insts.last._2.asInstanceOf[CallInst].tailcall)
  }
}
