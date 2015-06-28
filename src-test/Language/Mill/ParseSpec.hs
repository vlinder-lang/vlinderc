module Language.Mill.ParseSpec where

import Data.Either (rights)
import Language.Mill.Parse (parameter, parameterList, blockStmt, subDecl)
import Language.Mill.AST (Type(..), Name(..), Parameter(..), Decl(..), Expr(..))
import Test.Hspec (describe, it, shouldBe, Spec)
import Text.Parsec (eof, parse)

-- TODO move these
voidType = NamedType $ UnqualifiedName "Void"
emptyBlock = BlockExpr []
makeType name = NamedType $ UnqualifiedName name

spec :: Spec
spec = do
    describe "Language.Mill.Lex.parameter" $ do
      it "parses a parameter with a type" $ do
        rights [parse (parameter <* eof) "" "a: b"] `shouldBe` [Parameter "a" (NamedType $ UnqualifiedName "b")]

    describe "Language.Mill.Lex.parameterList" $ do
      it "parses an empy parameter list" $ do
        rights [parse (parameterList <* eof) "" "()"] `shouldBe` [[]]

      it "parses a parameter list with a single element" $ do
        rights [parse (parameterList <* eof) "" "(a: b)"] `shouldBe` [[Parameter "a" (makeType "b")]]

      it "parses a parameter list with multiple elements" $ do
        rights [parse (parameterList <* eof) "" "(a: b, c: d, e: f)"] `shouldBe` [[Parameter "a" (makeType "b"), Parameter "c" (makeType "d"), Parameter "e" (makeType "f")]]

      it "allows a trailing comma" $ do
        rights [parse (parameterList <* eof) "" "(a: b,)"] `shouldBe` [[Parameter "a" (makeType "b")]]

    describe "Language.Mill.Lex.blockStmt" $ do
      it "lexes an empty block" $ do
        rights [parse (blockStmt <* eof) "" "{}"] `shouldBe` [BlockExpr []]
        rights [parse (blockStmt <* eof) "" "{ }"] `shouldBe` [BlockExpr []]

    describe "Language.Mill.Lex.subDecl" $ do
      it "lexes an empty sub declaration" $ do
        rights [parse (subDecl <* eof) "" "sub foo(): Void { }"] `shouldBe` [SubDecl "foo" [] voidType emptyBlock]

      it "lexes an sub declaration with a parameter" $ do
        rights [parse (subDecl <* eof) "" "sub foo(a: b): Void { }"] `shouldBe` [SubDecl "foo" [Parameter "a" (makeType "b")] voidType emptyBlock]
