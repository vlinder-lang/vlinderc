module Language.Mill.NameSpec where

import qualified Data.Map as Map
import Language.Mill.AST
import Language.Mill.AST.ID (ID(..))
import Language.Mill.Name
import Language.Mill.Module (ModuleName(..))
import Test.Hspec (describe, it, shouldBe, Spec)

millLog :: Module
millLog = Module [ StructDecl (ID 12) "Record" [(Field "message" (NamedType (ID 13) (UnqualifiedName "__String")))]
                 , AliasDecl (ID 7) "Logger" (SubType (ID 8) [NamedType (ID 11) (UnqualifiedName "Record")] (TupleType (ID 9) []))
                 , SubDecl (ID 1) "info" [(Parameter (ID 5) "logger" (NamedType (ID 6) (UnqualifiedName "Logger")))] (TupleType (ID 2) []) (BlockExpr (ID 3) [ExprStmt (CallExpr (ID 10) (NameExpr (ID 4) (UnqualifiedName "logger")) [])])
                 ]

spec :: Spec
spec = do
    describe "resolveNamesInModules" $ do
      it "works" $ do
            let modules = Map.fromList [ (ModuleName ["mill", "log"], millLog)
                                       ]
            let expected = Map.fromList [ (ID 4, DeclSymbol (ID 5))
                                        , (ID 6, DeclSymbol (ID 7))
                                        , (ID 11, DeclSymbol (ID 12))
                                        , (ID 13, StringTypeSymbol)
                                        ]
            (resolveNamesInModules modules) `shouldBe` expected
