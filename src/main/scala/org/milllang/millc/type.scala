package org.milllang.millc

case class TypeError(reason: String) extends Exception(reason)

object TypeError {
  def couldNotUnify(a: Type, b: Type): TypeError =
    TypeError(s"could not unify '${a.format}' and '${b.format}'")
}

sealed abstract class Type {
  def equal(other: Type)(implicit context: Context): Boolean =
    resolve == other.resolve

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
  }

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

case class Context(
  typeDecls: Map[(ModuleName, String), TypeDecl],
  globalTypes: Map[(ModuleName, String), Type]
)

object Type {
  def analyze(modules: Vector[Module]): Unit = {
    implicit var context = Context(Map(), Map())
    context = populateTypeDecls(modules)
    context = populateGlobalTypes(modules)
  }

  private def populateTypeDecls(modules: Vector[Module])(implicit context: Context): Context = {
    val typeDecls = for {
      module <- modules
      decl <- module.decls
      typeDecl <- decl match {
        case StructDecl(name, astFields) =>
          val structFields = astFields map { case (name, typeExpr) =>
            (name, typeExprToType(typeExpr))
          }
          Vector((module.name, name) -> StructTypeDecl(name, structFields))
        case UnionDecl(name, astConstructors) =>
          val unionConstructors = astConstructors map { case (name, parameters) =>
            (name, parameters map typeExprToType)
          }
          Vector((module.name, name) -> UnionTypeDecl(name, unionConstructors))
        case AliasDecl(name, underlyingType) =>
          Vector((module.name, name) -> AliasTypeDecl(name, typeExprToType(underlyingType)))
        case _ =>
          Vector()
      }
    } yield typeDecl
    context.copy(typeDecls = context.typeDecls ++ typeDecls)
  }

  private def populateGlobalTypes(modules: Vector[Module])(implicit context: Context): Context = {
    var globalTypes = for {
      module <- modules
      decl <- module.decls
      globalType <- decl match {
        case SubDecl(name, valueParameters, returnTypeExpr, _) =>
          val parameterTypes = valueParameters map { case (_, typeExpr) => typeExprToType(typeExpr) }
          val returnType = typeExprToType(returnTypeExpr)
          Vector((module.name, name) -> SubType(parameterTypes, returnType))
        case _ =>
          Vector()
      }
    } yield globalType
    context.copy(globalTypes = context.globalTypes ++ globalTypes)
  }

  def analyze(expr: Expr)(implicit context: Context): Unit = expr match {
    case NameExpr(name) =>
      ???
    case BlockExpr() =>
      expr.`type` = TupleType()
    case BlockExpr(body @ _*) =>
      val init :+ last = body
      for (expr <- init) {
        analyze(expr)
        unify(expr.`type`, TupleType())
      }
      analyze(last)
      expr.`type` = last.`type`
    case CallExpr(callee, arguments) =>
      ???
    case StringLiteralExpr(_) =>
      expr.`type` = StringType
    case StructLiteralExpr(struct, fields) =>
      ???
  }

  def unify(a: Type, b: Type)(implicit context: Context): Unit =
    (a.resolve, b.resolve) match {
      case (a: VariableType, b) if !(a equal b) =>
        // TODO: throw recursive unification error if a occurs in b
        a.instance = Some(b)
      case (a, b: VariableType) =>
        unify(b, a)
      case (a, b) if a equal b =>
        ()
      case _ =>
        throw TypeError.couldNotUnify(a, b)
    }

  def typeExprToType(typeExpr: TypeExpr): Type = typeExpr match {
    case NameTypeExpr(name) =>
      ???
    case TupleTypeExpr(elementTypeExprs @ _*) =>
      TupleType(elementTypeExprs map typeExprToType: _*)
    case SubTypeExpr(parameterTypeExprs, returnTypeExpr) =>
      SubType(parameterTypeExprs map typeExprToType, typeExprToType(returnTypeExpr))
  }
}
