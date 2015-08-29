package org.vlinderlang.vlinderc.parse

import org.vlinderlang.vlinderc.ast._
import org.vlinderlang.vlinderc.ModuleName
import scala.util.parsing.combinator.Parsers

private[parse] object Parser extends Parsers {
  override type Elem = Token

  private def identifier: Parser[String] =
    accept("identifier", { case Identifier(name) => name })

  private def stringLiteral: Parser[String] =
    accept("string literal", { case StringLiteral(value) => value })

  def name: Parser[Name] = Vector(
    for { m <- identifier; _ <- Period; n <- identifier } yield QualifiedName(m, n),
    identifier ^^ UnqualifiedName
  ).reduce(_ | _)

  def decl: Parser[Decl] = Vector(
    aliasDecl,
    importDecl,
    subDecl
  ).reduce(_ | _)

  def aliasDecl: Parser[Decl] = for {
    _ <- Typealias
    name <- identifier
    _ <- Eq
    underlyingType <- typeExpr
    _ <- Semicolon
  } yield AliasDecl(name, underlyingType)

  def importDecl: Parser[Decl] = {
    def moduleName = rep1sep(identifier, Period) ^^ (ModuleName(_: _*))
    Import ~> moduleName <~ Semicolon ^^ ImportDecl
  }

  def subDecl: Parser[Decl] = {
    def param: Parser[(String, TypeExpr)] = for {
      name <- identifier
      _ <- Colon
      type_ <- typeExpr
    } yield (name, type_)
    for {
      _ <- Sub
      name <- identifier
      params <- LeftParen ~> param.* <~ RightParen ^^ (_.toVector)
      _ <- Colon
      returnType <- typeExpr
      body <- blockExpr
      _ <- Semicolon
    } yield SubDecl(name, params, returnType, body)
  }

  def expr: Parser[Expr] = callExpr

  def callExpr: Parser[Expr] = {
    val suffix = LeftParen ~> repsep(expr, Comma) <~ RightParen ^^ (_.toVector)
    for {
      callee <- primaryExpr
      suffixes <- suffix.*
    } yield suffixes.foldLeft(callee) { (callee, suffix) => CallExpr(callee, suffix) }
  }

  def primaryExpr: Parser[Expr] = Vector(
    nameExpr,
    blockExpr,
    stringLiteralExpr,
    structLiteralExpr
  ).reduce(_ | _)

  def nameExpr: Parser[Expr] =
    name ^^ NameExpr

  def blockExpr: Parser[Expr] =
    LeftBrace ~> (expr <~ Semicolon).* <~ RightBrace ^^ (BlockExpr(_: _*))

  def stringLiteralExpr: Parser[Expr] =
    stringLiteral ^^ StringLiteralExpr

  def structLiteralExpr: Parser[Expr] = {
    def field: Parser[(String, Expr)] = for {
      name <- identifier
      _ <- Colon
      value <- expr
    } yield (name, value)
    for {
      _ <- Mk
      struct <- typeExpr
      fields <- LeftBrace ~> field.* <~ RightBrace ^^ (_.toVector)
    } yield StructLiteralExpr(struct, fields)
  }

  def typeExpr: Parser[TypeExpr] = Vector(
    nameTypeExpr,
    subTypeExpr,
    tupleTypeExpr
  ).reduce(_ | _)

  def nameTypeExpr: Parser[TypeExpr] =
    name ^^ NameTypeExpr

  def tupleTypeExpr: Parser[TypeExpr] =
    LeftParen ~> repsep(typeExpr, Comma) <~ RightParen ^^ (TupleTypeExpr(_: _*))

  def subTypeExpr: Parser[TypeExpr] = for {
    paramTypes <- LeftParen ~> repsep(typeExpr, Comma) <~ RightParen ^^ (_.toVector)
    _ <- EqGT
    returnType <- typeExpr
  } yield SubTypeExpr(paramTypes, returnType)

  def module(name: ModuleName): Parser[Module] =
    decl.* ^^ { decls => Module(name, decls.toVector) }
}
