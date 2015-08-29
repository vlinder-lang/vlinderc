package org.vlinderlang.vlinderc

package object name {
  import org.vlinderlang.vlinderc.ast._
  import org.vlinderlang.vlinderc.ModuleName

  private implicit val canSetSymbol = CanSetSymbol

  case class NameResolutionError(message: String) extends Exception(message)

  case class Context(
    module: ModuleName,
    modules: Set[ModuleName],
    globals: Map[(ModuleName, String), Symbol],
    scope: Scope
  )

  case class Scope(parent: Option[Scope], symbols: Map[String, Symbol]) {
    def derive: Scope = Scope(Some(this), Map.empty)

    def add(name: String, symbol: Symbol) =
      if (symbols contains name)
        throw NameResolutionError(s"name '$name' is already defined")
      else
        copy(symbols = symbols + (name -> symbol))

    def get(name: String): Symbol =
      if (symbols contains name)
        symbols(name)
      else if (!parent.isEmpty)
        parent.get.get(name)
      else
        throw NameResolutionError(s"name '$name' is not in scope")
  }

  object Scope {
    def empty: Scope = Scope(None, Map.empty)

    def prelude: Scope = empty.copy(symbols = Map(
      "__String" -> StringTypeSymbol
    ))
  }

  def resolve(modules: Vector[Module]): Unit = {
    val moduleNames = modules.map(_.name).toSet
    val globals = findGlobals(modules)
    for (module <- modules) {
      val context = Context(module.name, moduleNames, globals, Scope.prelude.derive)
      module.decls.foldLeft(context) { (context, decl) =>
        resolveDecl(decl)(context)
      }
    }
  }

  private def resolveDecl(decl: Decl)(implicit context: Context): Context =
    decl match {
      case ImportDecl(module) =>
        if (!(context.modules contains module)) {
          throw NameResolutionError(
            s"module '${module.segments.mkString(".")}' does not exist"
          )
        }
        val symbol = ImportSymbol(module)
        context.copy(scope = context.scope.add(module.segments.last, symbol))

      case StructDecl(name, fields) =>
        for ((_, typeExpr) <- fields) {
          resolveTypeExpr(typeExpr)
        }
        val symbol = MemberTypeSymbol(context.module, name)
        context.copy(scope = context.scope.add(name, symbol))

      case UnionDecl(name, constructors) =>
        for ((_, paramTypeExprs) <- constructors;
             paramTypeExpr <- paramTypeExprs) {
          resolveTypeExpr(paramTypeExpr)
        }
        val symbol = MemberTypeSymbol(context.module, name)
        context.copy(scope = context.scope.add(name, symbol))

      case AliasDecl(name, typeExpr) =>
        resolveTypeExpr(typeExpr)
        val symbol = MemberTypeSymbol(context.module, name)
        context.copy(scope = context.scope.add(name, symbol))

      case SubDecl(name, valueParams, returnTypeExpr, body) =>
        for ((_, typeExpr) <- valueParams) {
          resolveTypeExpr(typeExpr)
        }
        resolveTypeExpr(returnTypeExpr)
        val symbol = MemberValueSymbol(context.module, name)
        val result = context.copy(scope = context.scope.add(name, symbol))
        val bodyScope = valueParams.foldLeft(context.scope.derive) { (scope, param) =>
          scope.add(param._1, ValueParamSymbol(param._1))
        }
        resolveExpr(body)(result.copy(scope = result.scope.derive))
        result
    }

  private def resolveTypeExpr(typeExpr: TypeExpr)(implicit context: Context): Unit =
    typeExpr match {
      case e @ NameTypeExpr(name) =>
        e.symbol = resolveName(name)
      case TupleTypeExpr(elementTypes @ _*) =>
        elementTypes foreach resolveTypeExpr
      case SubTypeExpr(paramTypes, returnType) =>
        paramTypes foreach resolveTypeExpr
        resolveTypeExpr(returnType)
    }

  private def resolveExpr(expr: Expr)(implicit context: Context): Unit =
    expr match {
      case e @ NameExpr(name) =>
        e.symbol = resolveName(name)
      case BlockExpr(exprs @ _*) =>
        exprs foreach resolveExpr
      case CallExpr(callee, arguments) =>
        resolveExpr(callee)
        arguments foreach resolveExpr
      case _: StringLiteralExpr =>
      case StructLiteralExpr(struct, fields) =>
        resolveTypeExpr(struct)
        fields.map(_._2) foreach resolveExpr
    }

  private def resolveName(name: Name)(implicit context: Context): Symbol =
    name match {
      case QualifiedName(module, name) =>
        context.scope.get(module) match {
          case ImportSymbol(moduleName) =>
            context.globals.get((moduleName, name)) match {
              case Some(symbol) => symbol
              case None =>
                throw NameResolutionError(
                  s"module ${moduleName.segments.mkString(".")} does not export name '$name'"
                )
            }
          case _ =>
            throw NameResolutionError(s"name '$name' does not refer to a module")
        }
      case UnqualifiedName(name) =>
        context.scope.get(name)
    }

  private def findGlobals(modules: Vector[Module]): Map[(ModuleName, String), Symbol] = {
    val globals = for {
      module <- modules
      decl <- module.decls
    } yield decl match {
      case _: ImportDecl => None
      case StructDecl(name, _) =>
        Some((module.name, name) -> MemberTypeSymbol(module.name, name))
      case UnionDecl(name, _) =>
        Some((module.name, name) -> MemberTypeSymbol(module.name, name))
      case AliasDecl(name, _) =>
        Some((module.name, name) -> MemberTypeSymbol(module.name, name))
      case SubDecl(name, _, _, _) =>
        Some((module.name, name) -> MemberValueSymbol(module.name, name))
    }
    globals.flatten.toMap
  }
}
