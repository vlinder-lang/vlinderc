package org.vlinderlang.vlinderc

package object parse {
  import org.vlinderlang.vlinderc.ast
  import org.vlinderlang.vlinderc.ModuleName
  import scala.util.parsing.input.{CharSequenceReader, NoPosition, Position, Reader}

  case class SyntaxError() extends Exception

  private def collapseNewlines(tokens: Vector[Token]): Vector[Token] = tokens match {
    case Newline +: Newline +: rest => collapseNewlines(Newline +: rest)
    case t +: ts => t +: collapseNewlines(ts)
    case Vector() => Vector()
  }

  private def semicolonAfter(token: Token): Boolean = token match {
    case RightBrace => true
    case RightParen => true
    case _: Identifier => true
    case _: StringLiteral => true
    case _ => false
  }

  private def semicolonBefore(token: Token): Boolean = token match {
    case EOF => true
    case LeftBrace => true
    case LeftParen => true
    case Import => true
    case RightBrace => true
    case Struct => true
    case Sub => true
    case Union => true
    case Typealias => true
    case _: Identifier => true
    case _: StringLiteral => true
    case _ => false
  }

  private def insertSemicolons(tokens: Vector[Token]): Vector[Token] = tokens match {
    case Newline +: rest =>
      insertSemicolons(rest)
    case t +: Newline +: u +: rest if semicolonAfter(t) && semicolonBefore(u) =>
      t +: Semicolon +: insertSemicolons(u +: rest)
    case t +: rest =>
      t +: insertSemicolons(rest)
    case Vector() =>
      Vector()
  }

  def lex(text: CharSequence): Vector[Token] = {
    val parser = Lexer.space ~> Lexer.token.* ^^ (_.toVector)
    val reader = new CharSequenceReader(text)
    parser(reader) match {
      case Lexer.Success(tokens, rest) if rest.atEnd =>
        insertSemicolons(collapseNewlines(tokens :+ EOF)).init
      case _ =>
        throw SyntaxError()
    }
  }

  def parse(name: ModuleName, tokens: Vector[Token]): ast.Module = {
    case class Reader(tokens: Vector[Token]) extends scala.util.parsing.input.Reader[Token] {
      override def atEnd: Boolean = tokens.isEmpty
      override def first: Token = tokens.head
      override def rest: Reader = Reader(tokens.tail)
      override def pos: Position = NoPosition
    }
    val reader = Reader(tokens)
    Parser.module(name)(reader) match {
      case Parser.Success(module, rest) if rest.atEnd =>
        module
      case _ =>
        throw SyntaxError()
    }
  }
}
