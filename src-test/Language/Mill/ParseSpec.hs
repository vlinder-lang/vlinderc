module Language.Mill.ParseSpec where

import Control.Applicative ((<*))
import Data.Either (rights)
import Language.Mill.AST
import Language.Mill.Parse
import Test.Hspec (describe, it, shouldBe, Spec)
import Text.Parsec (eof, runParser)

typeID = TypeID (-1)
declID = DeclID (-1)
exprID = ExprID (-1)

-- TODO move these
voidType = NamedType typeID (UnqualifiedName "Void")
exampleSubType = SubType typeID [(NamedType typeID (UnqualifiedName "A")), (NamedType typeID (UnqualifiedName "B"))] (NamedType typeID (UnqualifiedName "C"))
aType = NamedType typeID (UnqualifiedName "A")
bType = NamedType typeID (UnqualifiedName "B")
cType = NamedType typeID (UnqualifiedName "C")
dType = NamedType typeID (UnqualifiedName "D")
emptyBlock = BlockExpr exprID []
makeType name = NamedType typeID (UnqualifiedName name)
stringFoo = StringLiteralExpr "foo"
stringBar = StringLiteralExpr "bar"
abcQualifiedName = QualifiedName (ModuleName ["a", "b"]) "c"
abcType = NamedType abcQualifiedName
helloWorldSource = "import mill.log\n" ++
                   "\n" ++
                   "sub main(console: log.Logger): () {\n" ++
                   "    log.info(console, \"Hello, world!\")" ++
                   "}\n"
helloWorldAST = Module [ ImportDecl declID (ModuleName ["mill", "log"])
                       , SubDecl declID
                                 "main"
                                 [Parameter "console" (NamedType typeID (QualifiedName (ModuleName ["log"]) "Logger"))]
                                 (TupleType typeID [])
                                 (BlockExpr exprID [ExprStmt $ CallExpr exprID
                                                                        (NameExpr exprID (QualifiedName (ModuleName ["log"]) "info"))
                                                                        [NameExpr exprID (UnqualifiedName "console"), StringLiteralExpr exprID "Hello, world!"]])
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
        rights [runParser (name <* eof) 0 "" "a.b.c"] `shouldBe` [abcQualifiedName]

    describe "Language.Mill.Parse.parameter" $ do
      it "parses a parameter with a type" $ do
        rights [runParser (parameter <* eof) 0 "" "a: b"] `shouldBe` [Parameter "a" (NamedType typeID (UnqualifiedName "b"))]

    describe "Language.Mill.Parse.parameterList" $ do
      it "parses an empy parameter list" $ do
        rights [runParser (parameterList <* eof) 0 "" "()"] `shouldBe` [[]]

      it "parses a parameter list with a single element" $ do
        rights [runParser (parameterList <* eof) 0 "" "(a: b)"] `shouldBe` [[Parameter "a" (makeType "b")]]

      it "parses a parameter list with multiple elements" $ do
        rights [runParser (parameterList <* eof) 0 "" "(a: b, c: d, e: f)"] `shouldBe` [[Parameter "a" (makeType "b"), Parameter "c" (makeType "d"), Parameter "e" (makeType "f")]]

      it "allows a trailing comma" $ do
        rights [runParser (parameterList <* eof) 0 "" "(a: b,)"] `shouldBe` [[Parameter "a" (makeType "b")]]

    describe "Language.Mill.Parse.type_" $ do
      it "parses named types" $ do
        rights [runParser (type_ <* eof) 0 "" "A"] `shouldBe` [aType]
        rights [runParser (type_ <* eof) 0 "" "m.A"] `shouldBe` [NamedType typeID (QualifiedName (ModuleName ["m"]) "A")]
        rights [runParser (type_ <* eof) 0 "" "m.n.A"] `shouldBe` [NamedType typeID (QualifiedName (ModuleName ["m", "n"]) "A")]

      it "parses sub types" $ do
        rights [runParser (type_ <* eof) 0 "" "(A) => B"] `shouldBe` [SubType typeID [aType] bType]
        rights [runParser (type_ <* eof) 0 "" "(A, B) => C"] `shouldBe` [SubType typeID [aType, bType] cType]
        rights [runParser (type_ <* eof) 0 "" "(A, B) => (C) => D"] `shouldBe` [SubType typeID [aType, bType] (SubType typeID [cType] dType)]

      it "parses tuple types" $ do
        rights [runParser (type_ <* eof) 0 "" "()"] `shouldBe` [TupleType typeID []]
        rights [runParser (type_ <* eof) 0 "" "(A)"] `shouldBe` [TupleType typeID [aType]]
        rights [runParser (type_ <* eof) 0 "" "(A, B)"] `shouldBe` [TupleType typeID [aType, bType]]

    describe "Language.Mill.Parse.blockExpr" $ do
      it "parses an empty block" $ do
        rights [runParser (blockExpr <* eof) 0 "" "{}"] `shouldBe` [BlockExpr exprID []]
        rights [runParser (blockExpr <* eof) 0 "" "{ }"] `shouldBe` [BlockExpr exprID []]

    describe "Language.Mill.Parse.aliasDecl" $ do
      it "parses alias decls" $ do
        rights [runParser (aliasDecl <* eof) 0 "" "alias T = (A, B) => C"] `shouldBe` [AliasDecl declID "T" exampleSubType]

    describe "Language.Mill.Parse.structDecl" $ do
      it "parses struct decls" $ do
        rights [runParser (structDecl <* eof) 0 "" "struct T { }"] `shouldBe` [StructDecl declID "T" []]
        rights [runParser (structDecl <* eof) 0 "" "struct T { x: A }"] `shouldBe` [StructDecl declID "T" [Field "x" aType]]
        rights [runParser (structDecl <* eof) 0 "" "struct T { x: A y: B }"] `shouldBe` [StructDecl declID "T" [Field "x" aType, Field "y" bType]]

    describe "Language.Mill.Parse.structLitExpr" $ do
      it "parses an empty struct literal" $ do
        rights [runParser (structLitExpr <* eof) 0 "" "A{}"] `shouldBe` [StructLiteralExpr aType []]

      it "parses a struct literal with one value" $ do
        rights [runParser (structLitExpr <* eof) 0 "" "A{foo: \"foo\"}"] `shouldBe` [StructLiteralExpr aType [FieldValue "foo" stringFoo]]

      it "parses a struct literal with values" $ do
        rights [runParser (structLitExpr <* eof) 0 "" "A{foo: \"foo\", bar :\"bar\"}"] `shouldBe` [StructLiteralExpr aType [FieldValue "foo" stringFoo, FieldValue "bar" stringBar]]


    describe "Language.Mill.Parse.subDecl" $ do
      it "parses an empty sub declaration" $ do
        rights [runParser (subDecl <* eof) 0 "" "sub foo(): Void { }"] `shouldBe` [SubDecl declID "foo" [] voidType emptyBlock]

      it "parses an sub declaration with a parameter" $ do
        rights [runParser (subDecl <* eof) 0 "" "sub foo(a: b): (A, B) => C { }"] `shouldBe` [SubDecl declID "foo" [Parameter "a" (makeType "b")] exampleSubType emptyBlock]

    describe "Language.Mill.Parse.foreignSubDecl" $ do
      it "parses foreign sub declarations" $ do
        rights [runParser (foreignSubDecl <* eof) 0 "" "foreign \"./console.js\" sub ecmascript.returnCall info(message: String): ()"] `shouldBe` [ForeignSubDecl declID (ForeignLibrary "./console.js") (CallingConvention (QualifiedName (ModuleName ["ecmascript"]) "returnCall")) "info" [Parameter "message" (NamedType typeID (UnqualifiedName "String"))] (TupleType typeID [])]
