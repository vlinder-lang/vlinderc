package org.vlinderlang.vlinderc.ast

import org.vlinderlang.vlinderc.ModuleName
import org.vlinderlang.vlinderc.name.Symbol
import org.vlinderlang.vlinderc.`type`.Type

object CanSetSymbol
object CanSetType

case class Module(name: ModuleName, decls: Vector[Decl])

sealed abstract class Name
case class QualifiedName(module: String, name: String) extends Name
case class UnqualifiedName(name: String) extends Name

sealed abstract class Decl
case class ImportDecl(module: ModuleName) extends Decl
case class StructDecl(name: String, fields: Vector[(String, TypeExpr)]) extends Decl
case class UnionDecl(name: String, constructors: Vector[(String, Vector[TypeExpr])]) extends Decl
case class AliasDecl(name: String, underlyingType: TypeExpr) extends Decl
case class SubDecl(name: String, valueParameters: Vector[(String, TypeExpr)], returnType: TypeExpr, body: Expr) extends Decl

sealed abstract class Expr {
  private var t: Type = null
  def type_=(`type`: Type)(implicit cst: CanSetType.type): Unit = t = `type`
  def `type`: Type = t
}
case class NameExpr(name: Name) extends Expr {
  private var s: Symbol = null
  def symbol_=(symbol: Symbol)(implicit css: CanSetSymbol.type): Unit = s = symbol
  def symbol: Symbol = s
}
case class BlockExpr(body: Expr*) extends Expr
case class CallExpr(callee: Expr, arguments: Vector[Expr]) extends Expr
case class BooleanLiteralExpr(value: Boolean) extends Expr
case class StringLiteralExpr(value: String) extends Expr
case class StructLiteralExpr(struct: TypeExpr, fields: Vector[(String, Expr)]) extends Expr

sealed abstract class TypeExpr
case class NameTypeExpr(name: Name) extends TypeExpr {
  private var s: Symbol = null
  def symbol_=(symbol: Symbol)(implicit css: CanSetSymbol.type): Unit = s = symbol
  def symbol: Symbol = s
}
case class TupleTypeExpr(elementTypes: TypeExpr*) extends TypeExpr
case class SubTypeExpr(parameterTypes: Vector[TypeExpr], returnType: TypeExpr) extends TypeExpr
