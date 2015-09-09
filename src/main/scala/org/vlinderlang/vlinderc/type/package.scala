package org.vlinderlang.vlinderc

package object `type` {
  import org.vlinderlang.vlinderc.ast._
  import org.vlinderlang.vlinderc.ModuleName
  import org.vlinderlang.vlinderc.name._

  private implicit val canSetType = CanSetType

  case class TypeError(reason: String) extends Exception(reason)

  object TypeError {
    def couldNotUnify(a: Type, b: Type)(implicit context: Context): TypeError =
      TypeError(s"could not unify '${a.format}' and '${b.format}'")

    def nonStruct(t: Type)(implicit context: Context): TypeError =
      TypeError(s"expected a struct type but got '${t.format}'")

    def badStructFieldNames(expected: Set[String], actual: Set[String]): TypeError =
      TypeError(s"expected fields {${expected.mkString(", ")}} but got {${actual.mkString(", ")}}")

    def isModuleNotType: TypeError =
      TypeError(s"this is a module, not a type")

    def isValueNotType: TypeError =
      TypeError(s"this is a value, not a type")

    def isModuleNotValue: TypeError =
      TypeError(s"this is a module, not a value")

    def isTypeNotValue: TypeError =
      TypeError(s"this is a type, not a value")
  }

  /**
   * @param typeDecls contains type declarations
   * @param globalTypes contains types of global subroutines
   * @param paramTypes contains types of parameters
   */
  case class Context(
    typeDecls: Map[(ModuleName, String), TypeDecl],
    globalTypes: Map[(ModuleName, String), Type],
    paramTypes: Map[String, Type]
  )

  /**
   * Adds types to all expressions.
   */
  def analyze(modules: Vector[Module]): Unit = {
    implicit var context = Context(Map(), Map(), Map())
    context = populateTypeDecls(modules)
    context = populateGlobalTypes(modules)
    analyzeDecls(modules)
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
        case UnionDecl(name, constructors) =>
          constructors map { case (ctorName, Vector()) =>
            (module.name, ctorName) -> NamedType(module.name -> name)
          }
        case _ =>
          Vector()
      }
    } yield globalType
    context.copy(globalTypes = context.globalTypes ++ globalTypes)
  }

  def analyzeDecls(modules: Vector[Module])(implicit context: Context): Unit = {
    for (module <- modules;
         decl <- module.decls) {
      decl match {
        case SubDecl(_, valueParams, returnTypeExpr, body) =>
          val bodyContext = context.copy(
            paramTypes = valueParams.map{case (n, t) => (n, typeExprToType(t))}.toMap
          )
          val returnType = typeExprToType(returnTypeExpr)
          analyze(body)(bodyContext)
          unify(body.`type`, returnType)
        case _ => ()
      }
    }
  }

  /**
   * Adds type to an expression and all its subexpressions.
   */
  def analyze(expr: Expr)(implicit context: Context): Unit = expr match {
    case e @ NameExpr(_) =>
      e.symbol match {
        case _: ImportSymbol => throw TypeError.isModuleNotValue
        case _: MemberTypeSymbol => throw TypeError.isTypeNotValue
        case MemberValueSymbol(module, name) =>
          e.`type` = context.globalTypes((module, name))
        case ValueParamSymbol(name) =>
          e.`type` = context.paramTypes(name)
        case StringTypeSymbol => throw TypeError.isTypeNotValue
      }
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
      val calleeType = {
        analyze(callee)
        callee.`type`
      }
      val argumentTypes = arguments map { argument =>
        analyze(argument)
        argument.`type`
      }
      val returnType = VariableType()
      unify(calleeType, SubType(argumentTypes, returnType))
      expr.`type` = returnType
    case StringLiteralExpr(_) =>
      expr.`type` = StringType
    case StructLiteralExpr(struct, fields) =>
      val type_ = typeExprToType(struct)
      val decl = type_.resolve match {
        case t @ NamedType(name) =>
          context.typeDecls(name) match {
            case d: StructTypeDecl => d
            case _ => throw TypeError.nonStruct(t)
          }
        case t => throw TypeError.nonStruct(t)
      }

      val expectedFieldNames = decl.fields.map(_._1).toSet
      val actualFieldNames = fields.map(_._1).toSet
      if (expectedFieldNames != actualFieldNames) {
        throw TypeError.badStructFieldNames(expectedFieldNames, actualFieldNames)
      }

      val correspondingFields = decl.fields.sortBy(_._1) zip fields.sortBy(_._1)
      for (((_, fieldType), (_, value)) <- correspondingFields) {
        analyze(value)
        unify(fieldType, value.`type`)
      }

      expr.`type` = type_
  }

  /**
   * Unifies two types.
   */
  def unify(a: Type, b: Type)(implicit context: Context): Unit =
    (a.prune, b.prune) match {
      case (a, b) if a equal b =>
        ()
      case (a @ TupleType(aElementTypes @ _*), b @ TupleType(bElementTypes @ _*)) =>
        if (aElementTypes.size != bElementTypes.size) {
          throw TypeError.couldNotUnify(a, b)
        }
        for ((a, b) <- (aElementTypes zip bElementTypes)) {
          unify(a, b)
        }
      case (a @ SubType(aParameterTypes, aReturnType), b @ SubType(bParameterTypes, bReturnType)) =>
        if (aParameterTypes.size != bParameterTypes.size) {
          throw TypeError.couldNotUnify(a, b)
        }
        for ((a, b) <- (aParameterTypes zip bParameterTypes)) {
          unify(a, b)
        }
        unify(aReturnType, bReturnType)
      case (ForallType(parameters, aBody), b) =>
        val skolemized = skolemize(parameters.toSet, aBody)
        unify(skolemized, b)
      case (a, b @ ForallType(_, _)) =>
        unify(b, a)
      case (a: VariableType, b) =>
        // TODO: throw recursive unification error if a occurs in b
        a.instance = Some(b)
      case (a, b: VariableType) =>
        unify(b, a)
      case _ =>
        throw TypeError.couldNotUnify(a, b)
    }

  /**
   * Replaces forall variable types by variable types recursively.
   */
  def skolemize(parameters: Set[ForallParameter], `type`: Type): Type = {
    var replaced = Map[ForallParameter, VariableType]()
    def go(`type`: Type): Type = `type` match {
      case t @ StringType =>
        t
      case TupleType(elementTypes @ _*) =>
        TupleType(elementTypes map go: _*)
      case SubType(parameterTypes, returnType) =>
        SubType(parameterTypes map go, go(returnType))
      case t @ NamedType(_) =>
        t
      case ForallType(forallParameters, in) =>
        ForallType(forallParameters, go(in))
      case ForallVariableType(parameter) if parameters(parameter) =>
        if (!(replaced contains parameter)) {
          replaced += (parameter -> VariableType())
        }
        replaced(parameter)
      case t @ ForallVariableType(_) =>
        t
    }
    go(`type`)
  }

  /**
   * Turns a type expression into a type.
   */
  def typeExprToType(typeExpr: TypeExpr): Type = typeExpr match {
    case e: NameTypeExpr =>
      e.symbol match {
        case _: ImportSymbol => throw TypeError.isModuleNotType
        case MemberTypeSymbol(module, member) => NamedType((module, member))
        case _: MemberValueSymbol => throw TypeError.isValueNotType
        case _: ValueParamSymbol => throw TypeError.isValueNotType
        case StringTypeSymbol => StringType
      }
    case TupleTypeExpr(elementTypeExprs @ _*) =>
      TupleType(elementTypeExprs map typeExprToType: _*)
    case SubTypeExpr(parameterTypeExprs, returnTypeExpr) =>
      SubType(parameterTypeExprs map typeExprToType, typeExprToType(returnTypeExpr))
  }
}
