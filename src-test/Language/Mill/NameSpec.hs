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

notInScope1 :: Module
notInScope1 = Module [AliasDecl (ID 1) "T" (NamedType (ID 2) (UnqualifiedName "B"))]

notInScope2 :: Module
notInScope2 = Module [SubDecl (ID 1) "g" [] (TupleType (ID 2) []) (BlockExpr (ID 3) [ExprStmt $ NameExpr (ID 4) (UnqualifiedName "f")])]

notInScope3 :: Module
notInScope3 = Module [SubDecl (ID 1) "g" [] (TupleType (ID 2) []) (BlockExpr (ID 3) [ExprStmt $ NameExpr (ID 4) (QualifiedName "m" "f")])]

notInScope4 :: Module
notInScope4 = Module [ AliasDecl (ID 5) "m" (TupleType (ID 6) [])
                     , SubDecl (ID 1) "g" [] (TupleType (ID 2) []) (BlockExpr (ID 3) [ExprStmt $ NameExpr (ID 4) (QualifiedName "m" "f")])
                     ]

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
        let modules = Map.fromList [(ModuleName ["notInScope1"], notInScope1)]
        resolveNamesInModule modules (ModuleName ["notInScope1"]) `shouldBe` failNotInScope "B"

        let modules = Map.fromList [(ModuleName ["notInScope2"], notInScope2)]
        resolveNamesInModule modules (ModuleName ["notInScope2"]) `shouldBe` failNotInScope "f"

        let modules = Map.fromList [(ModuleName ["notInScope3"], notInScope3)]
        resolveNamesInModule modules (ModuleName ["notInScope3"]) `shouldBe` failNotInScope "m"

        let modules = Map.fromList [(ModuleName ["notInScope4"], notInScope4)]
        resolveNamesInModule modules (ModuleName ["notInScope4"]) `shouldBe` failNotAModule "m"
