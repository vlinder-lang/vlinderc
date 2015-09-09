package org.vlinderlang.vlinderc.ssa

import java.io.{ByteArrayInputStream, File}
import scala.sys.process._

object dot {
  def renderGraphical(path: File, cfg: CFG): Unit = {
    val stdin = new ByteArrayInputStream(renderDOT(cfg).getBytes("UTF-8"))
    (Seq("dot", "-Tpng", s"-o$path") #< stdin).!
  }

  def renderDOT(cfg: CFG): String = {
    def inst2dot(inst: Inst): String = inst match {
      case CallInst(callee, arguments, tailcall) =>
        s"call $callee(${arguments.mkString(", ")}) [tail = $tailcall]"
      case LdargInst(index) =>
        s"ldarg $index"
      case LdgblInst(module, member) =>
        s"ldgbl ${module.segments.mkString(".")}.$member"
      case LdstrInst(value) =>
        "ldstr \"" + value + "\""
      case NewInst(t) =>
        s"new ${t.descriptor}"
      case RetInst(value) =>
        s"ret $value"
      case StfldInst(target, field, value) =>
        s"stfld $target.$field := $value"
    }

    val b = new StringBuilder
    b.append("digraph {")
    for ((blockID, block) <- cfg.blocks) {
      b.append(blockID)
      b.append(s"""
        [
          shape=plaintext
          label=<
            <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
              <TR>
                <TD COLSPAN="2">$blockID</TD>
              </TR>
      """)
      for ((instID, inst) <- block.insts) {
        b.append(s"""
          <TR>
            <TD ALIGN="right">$instID</TD>
            <TD ALIGN="left"><FONT FACE="Courier New">${inst2dot(inst)}</FONT></TD>
          </TR>
        """)
      }
      b.append("""
            </TABLE>
          >
        ]
      """)
    }
    b.append("entry [style=invis]\n")
    b.append(s"entry -> ${cfg.entry}\n")
    b.append("}")
    b.result
  }
}
