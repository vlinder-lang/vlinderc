package org.vlinderlang.vlinderc.parse

import scala.util.parsing.combinator.{Parsers, RegexParsers}

sealed abstract class Token

private[parse] case object EOF extends Token

case class Identifier(name: String) extends Token

case object Import extends Token
case object Sub extends Token

case class StringLiteral(value: String) extends Token

case object Colon extends Token
case object Comma extends Token
case object LeftBrace extends Token
case object LeftParen extends Token
private[parse] case object Newline extends Token
case object Period extends Token
case object RightBrace extends Token
case object RightParen extends Token
case object Semicolon extends Token

private[parse] object Lexer extends Parsers with RegexParsers {
  override type Elem = Char

  override def skipWhitespace: Boolean = false

  def space: Parser[Unit] = """ *""".r ^^^ (())

  def identifier: Parser[Token] =
    """[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ Identifier

  def keyword: Parser[Token] = Vector(
    "import" ^^^ Import,
    "sub" ^^^ Sub
  ).reduce(_ ||| _)

  def literal: Parser[Token] = Vector(
    """".*?"""".r ^^ { s => StringLiteral(s.substring(1, s.length - 1)) }
  ).reduce(_ ||| _)

  def punctuation: Parser[Token] = Vector(
    ":" ^^^ Colon,
    "," ^^^ Comma,
    "{" ^^^ LeftBrace,
    "(" ^^^ LeftParen,
    "\n" ^^^ Newline,
    "." ^^^ Period,
    "}" ^^^ RightBrace,
    ")" ^^^ RightParen,
    ";" ^^^ Semicolon
  ).reduce(_ ||| _)

  def token: Parser[Token] =
    (identifier ||| keyword ||| literal ||| punctuation) <~ space
}
