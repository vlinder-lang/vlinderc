module Language.Mill.NameSpec where

import qualified Data.Map as Map
import Language.Mill.AST
import Language.Mill.AST.ID (ID(..))
import Language.Mill.Name
import Language.Mill.Module (ModuleName(..))
import Test.Hspec (describe, it, shouldBe, Spec)

millLog :: Module
millLog = Module [ ImportDecl (ID 14) (ModuleName ["mill", "text"])
                 , StructDecl (ID 12) "Record" [(Field "message" (NamedType (ID 13) (QualifiedName "text" "String")))]
                 , AliasDecl (ID 7) "Logger" (SubType (ID 8) [NamedType (ID 11) (UnqualifiedName "Record")] (TupleType (ID 9) []))
                 , SubDecl (ID 1) "info" [(Parameter (ID 5) "logger" (NamedType (ID 6) (UnqualifiedName "Logger")))] (TupleType (ID 2) []) (BlockExpr (ID 3) [ExprStmt (CallExpr (ID 10) (NameExpr (ID 4) (UnqualifiedName "logger")) [])])
                 ]

millText :: Module
millText = Module [AliasDecl (ID 15) "String" (NamedType (ID 16) (UnqualifiedName "__String"))]

notInScope :: Module
notInScope = Module [AliasDecl (ID 1) "T" (NamedType (ID 2) (UnqualifiedName "B"))]

spec :: Spec
spec = do
    describe "resolveNamesInModule" $ do
      it "works" $ do
        let modules = Map.fromList [ (ModuleName ["mill", "log"], millLog)
                                   , (ModuleName ["mill", "text"], millText)
                                   ]
        let expected = Map.fromList [ (ID 4, DeclSymbol (ID 5))
                                    , (ID 6, DeclSymbol (ID 7))
                                    , (ID 11, DeclSymbol (ID 12))
                                    , (ID 13, DeclSymbol (ID 15))
                                    , (ID 16, StringTypeSymbol)
                                    ]
        resolveNamesInModule modules (ModuleName ["mill", "log"]) `shouldBe` Right expected

      it "returns error messages" $ do
        let modules = Map.fromList [(ModuleName ["notInScope"], notInScope)]
        resolveNamesInModule modules (ModuleName ["notInScope"]) `shouldBe` failNotInScope "B"
