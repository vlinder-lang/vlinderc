package org.vlinderlang.vlinderc.ssa

import org.vlinderlang.vlinderc.ModuleName
import scala.collection.immutable.ListMap

class BlockID
class InstID

case class Block(insts: ListMap[InstID, Inst])

sealed abstract class Inst
case class CallInst(callee: InstID, arguments: Vector[InstID]) extends Inst
case class LdargInst(index: Int) extends Inst
case class LdgblInst(module: ModuleName, name: String) extends Inst
case class LdstrInst(value: String) extends Inst
case class RetInst(value: InstID) extends Inst
