module Language.Mill.ParseSpec where

import Control.Applicative ((<*))
import Data.Either (rights)
import Language.Mill.AST
import Language.Mill.Parse
import Test.Hspec (describe, it, shouldBe, Spec)
import Text.Parsec (eof, parse)

-- TODO move these
voidType = NamedType $ UnqualifiedName "Void"
exampleSubType = SubType [(NamedType (UnqualifiedName "A")), (NamedType (UnqualifiedName "B"))] (NamedType (UnqualifiedName "C"))
aType = NamedType $ UnqualifiedName "A"
bType = NamedType $ UnqualifiedName "B"
cType = NamedType $ UnqualifiedName "C"
dType = NamedType $ UnqualifiedName "D"
emptyBlock = BlockExpr []
makeType name = NamedType $ UnqualifiedName name
helloWorldSource = "import mill.log\n" ++
                   "\n" ++
                   "sub main(console: log.Logger): () {\n" ++
                   "    log.info(console, \"Hello, world!\")" ++
                   "}\n"
helloWorldAST = Module [ ImportDecl (ModuleName ["mill", "log"])
                       , SubDecl "main"
                                 [Parameter "console" (NamedType (QualifiedName (ModuleName ["log"]) "Logger"))]
                                 (TupleType [])
                                 (BlockExpr [ExprStmt $ CallExpr (NameExpr (QualifiedName (ModuleName ["log"]) "info"))
                                                                 [NameExpr (UnqualifiedName "console"), StringLiteralExpr "Hello, world!"]])
                       ]

spec :: Spec
spec = do
    describe "Language.Mill.Parse.module_" $ do
      it "parses the hello world program" $ do
        rights [parse module_ "" helloWorldSource] `shouldBe` [helloWorldAST]

    describe "Language.Mill.Parse.name" $ do
      it "parses unqualified names" $ do
        rights [parse (name <* eof) "" "a"] `shouldBe` [UnqualifiedName "a"]

      it "parses qualified names" $ do
        rights [parse (name <* eof) "" "a.b.c"] `shouldBe` [QualifiedName (ModuleName ["a", "b"]) "c"]

    describe "Language.Mill.Parse.parameter" $ do
      it "parses a parameter with a type" $ do
        rights [parse (parameter <* eof) "" "a: b"] `shouldBe` [Parameter "a" (NamedType $ UnqualifiedName "b")]

    describe "Language.Mill.Parse.parameterList" $ do
      it "parses an empy parameter list" $ do
        rights [parse (parameterList <* eof) "" "()"] `shouldBe` [[]]

      it "parses a parameter list with a single element" $ do
        rights [parse (parameterList <* eof) "" "(a: b)"] `shouldBe` [[Parameter "a" (makeType "b")]]

      it "parses a parameter list with multiple elements" $ do
        rights [parse (parameterList <* eof) "" "(a: b, c: d, e: f)"] `shouldBe` [[Parameter "a" (makeType "b"), Parameter "c" (makeType "d"), Parameter "e" (makeType "f")]]

      it "allows a trailing comma" $ do
        rights [parse (parameterList <* eof) "" "(a: b,)"] `shouldBe` [[Parameter "a" (makeType "b")]]

    describe "Language.Mill.Parse.type_" $ do
      it "parses named types" $ do
        rights [parse (type_ <* eof) "" "A"] `shouldBe` [aType]
        rights [parse (type_ <* eof) "" "m.A"] `shouldBe` [NamedType (QualifiedName (ModuleName ["m"]) "A")]
        rights [parse (type_ <* eof) "" "m.n.A"] `shouldBe` [NamedType (QualifiedName (ModuleName ["m", "n"]) "A")]

      it "parses sub types" $ do
        rights [parse (type_ <* eof) "" "(A) => B"] `shouldBe` [SubType [aType] bType]
        rights [parse (type_ <* eof) "" "(A, B) => C"] `shouldBe` [SubType [aType, bType] cType]
        rights [parse (type_ <* eof) "" "(A, B) => (C) => D"] `shouldBe` [SubType [aType, bType] (SubType [cType] dType)]

      it "parses tuple types" $ do
        rights [parse (type_ <* eof) "" "()"] `shouldBe` [TupleType []]
        rights [parse (type_ <* eof) "" "(A)"] `shouldBe` [TupleType [aType]]
        rights [parse (type_ <* eof) "" "(A, B)"] `shouldBe` [TupleType [aType, bType]]

    describe "Language.Mill.Parse.blockExpr" $ do
      it "parses an empty block" $ do
        rights [parse (blockExpr <* eof) "" "{}"] `shouldBe` [BlockExpr []]
        rights [parse (blockExpr <* eof) "" "{ }"] `shouldBe` [BlockExpr []]

    describe "Language.Mill.Parse.subDecl" $ do
      it "parses an empty sub declaration" $ do
        rights [parse (subDecl <* eof) "" "sub foo(): Void { }"] `shouldBe` [SubDecl "foo" [] voidType emptyBlock]

      it "parses an sub declaration with a parameter" $ do
        rights [parse (subDecl <* eof) "" "sub foo(a: b): (A, B) => C { }"] `shouldBe` [SubDecl "foo" [Parameter "a" (makeType "b")] exampleSubType emptyBlock]
