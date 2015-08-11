package org.vlinderlang.vlinderc.parse

import org.scalatest.FlatSpec
import org.vlinderlang.vlinderc.ast._
import org.vlinderlang.vlinderc.ModuleName

class packageSpec extends FlatSpec {
  "lex" should "lex the empty input" in {
    assert(lex("") == Vector())
  }

  it should "lex the hello world program" in {
    val code = """
      import vlinder.log

      sub main(console: log.Logger): () {
        log.info(console, "Hello, world!")
      }
    """
    val expected = Vector(
      Import,
      Identifier("vlinder"),
      Period,
      Identifier("log"),
      Semicolon,
      Sub,
      Identifier("main"),
      LeftParen,
      Identifier("console"),
      Colon,
      Identifier("log"),
      Period,
      Identifier("Logger"),
      RightParen,
      Colon,
      LeftParen,
      RightParen,
      LeftBrace,
      Identifier("log"),
      Period,
      Identifier("info"),
      LeftParen,
      Identifier("console"),
      Comma,
      StringLiteral("Hello, world!"),
      RightParen,
      Semicolon,
      RightBrace,
      Semicolon
    )
    assert(lex(code) == expected)
  }

  "parse" should "parse the hello world program" in {
    val code = """
      import vlinder.log

      sub main(console: log.Logger): () {
        log.info(console, "Hello, world!")
      }
    """
    val expected = Module(
      ModuleName("main"),
      Vector(
        ImportDecl(ModuleName("vlinder", "log")),
        SubDecl(
          "main",
          Vector(("console", NameTypeExpr(QualifiedName("log", "Logger")))),
          TupleTypeExpr(),
          BlockExpr(
            CallExpr(
              NameExpr(QualifiedName("log", "info")),
              Vector(
                NameExpr(UnqualifiedName("console")),
                StringLiteralExpr("Hello, world!")
              )
            )
          )
        )
      )
    )
    assert(parse(ModuleName("main"), lex(code)) == expected)
  }
}
