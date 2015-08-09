package org.milllang.millc

sealed abstract class Type {
  def equal(other: Type)(implicit context: Context): Boolean =
    resolve == other.resolve

  def resolve(implicit context: Context): Type = this match {
    case t @ NamedType(name) =>
      context.typeDecls(name) match {
        case AliasTypeDecl(_, underlyingType) => underlyingType.resolve
        case _ => t
      }
    case t => t
  }

  def descriptor: String = this match {
    case StringType =>
      "S"
    case TupleType(elementTypes @ _*) =>
      s"T${elementTypes.map(_.descriptor).mkString("")};"
    case SubType(parameterTypes, returnType) =>
      s"F${parameterTypes.map(_.descriptor).mkString("")}${returnType.descriptor};"
    case NamedType(name) =>
      s"N${name._1.segments.mkString(".")}.${name._2};"
  }
}

case object StringType extends Type
case class TupleType(elementTypes: Type*) extends Type
case class SubType(parameterTypes: Vector[Type], returnType: Type) extends Type
case class NamedType(name: (ModuleName, String)) extends Type

sealed abstract class TypeDecl
case class StructTypeDecl(name: String, fields: Vector[(String, Type)]) extends TypeDecl
case class UnionTypeDecl(name: String, fields: Vector[(String, Vector[Type])]) extends TypeDecl
case class AliasTypeDecl(name: String, underlyingType: Type) extends TypeDecl

case class Context(typeDecls: Map[(ModuleName, String), TypeDecl])
