package org.vlinderlang.vlinderc.ssa

import org.vlinderlang.vlinderc.ModuleName
import org.vlinderlang.vlinderc.`type`.Type
import scala.collection.immutable.ListMap

sealed abstract class ID(prefix: String) {
  private val id = ID.nextID()
  override def toString = s"$prefix$id"
}

object ID {
  private var lastID = 0

  private def nextID(): Int = synchronized {
    lastID += 1
    lastID
  }
}

class BlockID extends ID("B")
class InstID extends ID("I")

case class CFG(entry: BlockID, blocks: Map[BlockID, Block]) {
  def incoming(blockID: BlockID): Set[BlockID] = Set.empty

  def outgoing(blockID: BlockID): Set[BlockID] = Set.empty
}

case class Block(insts: ListMap[InstID, Inst])

sealed abstract class Inst
case class CallInst(callee: InstID, arguments: Vector[InstID], tailcall: Boolean) extends Inst
case class LdargInst(index: Int) extends Inst
case class LdgblInst(module: ModuleName, member: String) extends Inst
case class LdstrInst(value: String) extends Inst
case class NewInst(`type`: Type) extends Inst
case class RetInst(value: InstID) extends Inst
case class StfldInst(target: InstID, field: String, value: InstID) extends Inst

class CFGBuilder {
  private var cfg = {
    val entryID = new BlockID
    CFG(entryID, Map(entryID -> Block(ListMap.empty)))
  }

  var currentBlockID = cfg.entry

  def block(): BlockID = {
    val blockID = new BlockID
    cfg = cfg.copy(blocks = cfg.blocks + (blockID -> Block(ListMap.empty)))
    currentBlockID = blockID
    blockID
  }

  def inst(inst: Inst): InstID = {
    val instID = new InstID
    val block = cfg.blocks(currentBlockID)
    val newBlock = block.copy(insts = block.insts + (instID -> inst))
    cfg = cfg.copy(blocks = cfg.blocks + (currentBlockID -> newBlock))
    instID
  }

  def result: CFG = cfg
}
