package org.milllang.millc.`type`

import org.milllang.millc.ModuleName

sealed abstract class Type {
  /**
   * Returns whether two types are equal.
   */
  def equal(other: Type)(implicit context: Context): Boolean =
    prune == other.prune

  /**
   * Resolves variable types recursively.
   */
  def prune(implicit context: Context): Type = resolve match {
    case t @ VariableType(_) =>
      t.instance match {
        case Some(u) => u.prune
        case None => t
      }
    case t @ StringType =>
      t
    case TupleType(elementTypes @ _*) =>
      TupleType(elementTypes map (_.prune): _*)
    case SubType(parameterTypes, returnType) =>
      SubType(parameterTypes map (_.prune), returnType.prune)
    case t @ NamedType(_) =>
      t
  }

  /**
   * Resolves type aliases recursively.
   */
  def resolve(implicit context: Context): Type = this match {
    case t @ VariableType(_) =>
      t.instance match {
        case Some(u) => u.resolve
        case None => t
      }
    case t @ StringType =>
      t
    case TupleType(elementTypes @ _*) =>
      TupleType(elementTypes map (_.resolve): _*)
    case SubType(parameterTypes, returnType) =>
      SubType(parameterTypes map (_.resolve), returnType.resolve)
    case t @ NamedType(name) =>
      context.typeDecls(name) match {
        case AliasTypeDecl(_, underlyingType) => underlyingType.resolve
        case _ => t
      }
    case t => t
  }

  /**
   * Returns the descriptor of the type.
   */
  def descriptor: String = this match {
    case VariableType(id) =>
      s"V$id"
    case StringType =>
      "S"
    case TupleType(elementTypes @ _*) =>
      s"T${elementTypes.map(_.descriptor).mkString("")};"
    case SubType(parameterTypes, returnType) =>
      s"F${parameterTypes.map(_.descriptor).mkString("")}${returnType.descriptor};"
    case NamedType(name) =>
      s"N${name._1.segments.mkString(".")}.${name._2};"
  }

  /**
   * Formats the type for error messages and other information.
   */
  def format: String = this match {
    case VariableType(id) =>
      s"__v$id"
    case StringType =>
      "mill.text.String"
    case TupleType(elementTypes @ _*) =>
      s"(${elementTypes.map(_.format).mkString(", ")})"
    case SubType(parameterTypes, returnType) =>
      s"(${parameterTypes.map(_.format).mkString(", ")}) => ${returnType.format}"
    case NamedType(name) =>
      s"${name._1.segments.mkString(".")}.${name._2}"
  }
}
case object StringType extends Type
case class VariableType(id: Int) extends Type {
  var instance: Option[Type] = None
}
object VariableType {
  private var lastID = 0
  def apply(): VariableType = synchronized {
    lastID += 1
    VariableType(lastID)
  }
}
case class TupleType(elementTypes: Type*) extends Type
case class SubType(parameterTypes: Vector[Type], returnType: Type) extends Type
case class NamedType(name: (ModuleName, String)) extends Type

sealed abstract class TypeDecl
case class StructTypeDecl(name: String, fields: Vector[(String, Type)]) extends TypeDecl
case class UnionTypeDecl(name: String, constructors: Vector[(String, Vector[Type])]) extends TypeDecl
case class AliasTypeDecl(name: String, underlyingType: Type) extends TypeDecl
