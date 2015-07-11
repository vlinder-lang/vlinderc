module Language.Mill.ParseSpec where

import Control.Applicative ((<*))
import Data.Either (rights)
import Language.Mill.AST
import Language.Mill.AST.ID (ID(..))
import Language.Mill.Module (ModuleName(..))
import Language.Mill.Parse
import Test.Hspec (describe, it, shouldBe, Spec)
import Text.Parsec (eof, runParser)

nodeID = ID (-1)

-- TODO move these
voidType = NamedType nodeID (UnqualifiedName "Void")
exampleSubType = SubType nodeID [(NamedType nodeID (UnqualifiedName "A")), (NamedType nodeID (UnqualifiedName "B"))] (NamedType nodeID (UnqualifiedName "C"))
aType = NamedType nodeID (UnqualifiedName "A")
bType = NamedType nodeID (UnqualifiedName "B")
cType = NamedType nodeID (UnqualifiedName "C")
dType = NamedType nodeID (UnqualifiedName "D")
emptyBlock = BlockExpr nodeID []
makeType name = NamedType nodeID (UnqualifiedName name)
helloWorldSource = "import mill.log\n" ++
                   "\n" ++
                   "sub main(console: log.Logger): () {\n" ++
                   "    log.info(console, \"Hello, world!\")" ++
                   "}\n"
helloWorldAST = Module [ ImportDecl nodeID (ModuleName ["mill", "log"])
                       , SubDecl nodeID
                                 "main"
                                 [Parameter nodeID "console" (NamedType nodeID (QualifiedName "log" "Logger"))]
                                 (TupleType nodeID [])
                                 (BlockExpr nodeID [ExprStmt $ CallExpr nodeID
                                                                        (NameExpr nodeID (QualifiedName "log" "info"))
                                                                        [NameExpr nodeID (UnqualifiedName "console"), StringLiteralExpr nodeID "Hello, world!"]])
                       ]

spec :: Spec
spec = do
    describe "Language.Mill.Parse.module_" $ do
      it "parses the hello world program" $ do
        rights [runParser module_ 0 "" helloWorldSource] `shouldBe` [helloWorldAST]

    describe "Language.Mill.Parse.name" $ do
      it "parses unqualified names" $ do
        rights [runParser (name <* eof) 0 "" "a"] `shouldBe` [UnqualifiedName "a"]

      it "parses qualified names" $ do
        rights [runParser (name <* eof) 0 "" "a.b"] `shouldBe` [QualifiedName "a" "b"]

    describe "Language.Mill.Parse.parameter" $ do
      it "parses a parameter with a type" $ do
        rights [runParser (parameter <* eof) 0 "" "a: b"] `shouldBe` [Parameter nodeID "a" (NamedType nodeID (UnqualifiedName "b"))]

    describe "Language.Mill.Parse.parameterList" $ do
      it "parses an empy parameter list" $ do
        rights [runParser (parameterList <* eof) 0 "" "()"] `shouldBe` [[]]

      it "parses a parameter list with a single element" $ do
        rights [runParser (parameterList <* eof) 0 "" "(a: b)"] `shouldBe` [[Parameter nodeID "a" (makeType "b")]]

      it "parses a parameter list with multiple elements" $ do
        rights [runParser (parameterList <* eof) 0 "" "(a: b, c: d, e: f)"] `shouldBe` [[Parameter nodeID "a" (makeType "b"), Parameter nodeID "c" (makeType "d"), Parameter nodeID "e" (makeType "f")]]

      it "allows a trailing comma" $ do
        rights [runParser (parameterList <* eof) 0 "" "(a: b,)"] `shouldBe` [[Parameter nodeID "a" (makeType "b")]]

    describe "Language.Mill.Parse.type_" $ do
      it "parses named types" $ do
        rights [runParser (type_ <* eof) 0 "" "A"] `shouldBe` [aType]
        rights [runParser (type_ <* eof) 0 "" "m.A"] `shouldBe` [NamedType nodeID (QualifiedName "m" "A")]

      it "parses sub types" $ do
        rights [runParser (type_ <* eof) 0 "" "(A) => B"] `shouldBe` [SubType nodeID [aType] bType]
        rights [runParser (type_ <* eof) 0 "" "(A, B) => C"] `shouldBe` [SubType nodeID [aType, bType] cType]
        rights [runParser (type_ <* eof) 0 "" "(A, B) => (C) => D"] `shouldBe` [SubType nodeID [aType, bType] (SubType nodeID [cType] dType)]

      it "parses tuple types" $ do
        rights [runParser (type_ <* eof) 0 "" "()"] `shouldBe` [TupleType nodeID []]
        rights [runParser (type_ <* eof) 0 "" "(A)"] `shouldBe` [TupleType nodeID [aType]]
        rights [runParser (type_ <* eof) 0 "" "(A, B)"] `shouldBe` [TupleType nodeID [aType, bType]]

    describe "Language.Mill.Parse.blockExpr" $ do
      it "parses an empty block" $ do
        rights [runParser (blockExpr <* eof) 0 "" "{}"] `shouldBe` [BlockExpr nodeID []]
        rights [runParser (blockExpr <* eof) 0 "" "{ }"] `shouldBe` [BlockExpr nodeID []]

    describe "Language.Mill.Parse.aliasDecl" $ do
      it "parses alias decls" $ do
        rights [runParser (aliasDecl <* eof) 0 "" "alias T = (A, B) => C"] `shouldBe` [AliasDecl nodeID "T" exampleSubType]

    describe "Language.Mill.Parse.structDecl" $ do
      it "parses struct decls" $ do
        rights [runParser (structDecl <* eof) 0 "" "struct T { }"] `shouldBe` [StructDecl nodeID "T" []]
        rights [runParser (structDecl <* eof) 0 "" "struct T { x: A }"] `shouldBe` [StructDecl nodeID "T" [Field "x" aType]]
        rights [runParser (structDecl <* eof) 0 "" "struct T { x: A y: B }"] `shouldBe` [StructDecl nodeID "T" [Field "x" aType, Field "y" bType]]

    describe "Language.Mill.Parse.subDecl" $ do
      it "parses an empty sub declaration" $ do
        rights [runParser (subDecl <* eof) 0 "" "sub foo(): Void { }"] `shouldBe` [SubDecl nodeID "foo" [] voidType emptyBlock]

      it "parses an sub declaration with a parameter" $ do
        rights [runParser (subDecl <* eof) 0 "" "sub foo(a: b): (A, B) => C { }"] `shouldBe` [SubDecl nodeID "foo" [Parameter nodeID "a" (makeType "b")] exampleSubType emptyBlock]

    describe "Language.Mill.Parse.foreignSubDecl" $ do
      it "parses foreign sub declarations" $ do
        rights [runParser (foreignSubDecl <* eof) 0 "" "foreign \"./console.js\" sub ecmascript.returnCall info(message: String): ()"] `shouldBe` [ForeignSubDecl nodeID (ForeignLibrary "./console.js") (CallingConvention (QualifiedName "ecmascript" "returnCall")) "info" [Parameter nodeID "message" (NamedType nodeID (UnqualifiedName "String"))] (TupleType nodeID [])]
