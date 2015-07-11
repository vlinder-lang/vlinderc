module Language.Mill.NameSpec where

import qualified Data.Map as Map
import Language.Mill.AST
import Language.Mill.AST.ID (ID(..))
import Language.Mill.Name
import Language.Mill.Module (ModuleName(..))
import Test.Hspec (describe, it, shouldBe, Spec)

millLog :: Module
millLog = Module [ SubDecl (ID 1) "info" [] (TupleType (ID 2) []) (BlockExpr (ID 3) [ExprStmt (NameExpr (ID 4) (UnqualifiedName "info"))])
                 ]

spec :: Spec
spec = do
    describe "resolveNamesInModules" $ do
      it "works" $ do
            let modules = Map.fromList [ (ModuleName ["mill", "log"], millLog)
                                       ]
            let expected = Map.fromList [ (ID 4, DeclSymbol (ID 1))
                                        ]
            (resolveNamesInModules modules) `shouldBe` expected
