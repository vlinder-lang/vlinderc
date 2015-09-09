package org.vlinderlang.vlinderc

package object ast2ssa {
  import org.vlinderlang.vlinderc.ast._
  import org.vlinderlang.vlinderc.ModuleName
  import org.vlinderlang.vlinderc.name._
  import org.vlinderlang.vlinderc.ssa._
  import org.vlinderlang.vlinderc.`type`._

  case class Context(valueParameters: Vector[String])

  def convert(modules: Vector[Module]): Map[(ModuleName, String), CFG] =
    modules.foldLeft(Map.empty[(ModuleName, String), CFG]) { (map, module) =>
      val cfgs = module.decls.collect {
        case sub: SubDecl => (module.name, sub.name) -> convertSub(sub)
      }
      map ++ cfgs
    }

  private def convertSub(sub: SubDecl): CFG = {
    implicit val b = new CFGBuilder
    implicit val c = Context(sub.valueParameters.map(_._1))
    val result = convertExpr(sub.body)
    b.inst(RetInst(result))
    b.result
  }

  private def convertExpr(expr: Expr)(implicit b: CFGBuilder, c: Context): InstID =
    expr match {
      case e: NameExpr =>
        e.symbol match {
          case MemberValueSymbol(module, member) =>
            b.inst(LdgblInst(module, member))
          case ValueParamSymbol(name) =>
            b.inst(LdargInst(c.valueParameters.indexOf(name)))
        }

      case BlockExpr() =>
        b.inst(NewInst(TupleType()))

      case BlockExpr(body @ _*) =>
        val init :+ last = body
        init foreach convertExpr
        convertExpr(last)

      case CallExpr(callee, arguments) =>
        val calleeID = convertExpr(callee)
        val argumentIDs = arguments map convertExpr
        b.inst(CallInst(calleeID, argumentIDs, tailcall = false))

      case StringLiteralExpr(value) =>
        b.inst(LdstrInst(value))

      case StructLiteralExpr(struct, fields) =>
        val resultID = b.inst(NewInst(typeExprToType(struct)))
        fields.foldLeft(resultID) { (resultID, field) =>
          val valueID = convertExpr(field._2)
          b.inst(StfldInst(resultID, field._1, valueID))
        }
    }
}
