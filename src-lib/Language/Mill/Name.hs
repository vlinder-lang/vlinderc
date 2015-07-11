module Language.Mill.Name where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Map (Map)
import Data.Set (Set)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Monoid (mconcat)
import Language.Mill.AST
import Language.Mill.AST.ID (ID)
import Language.Mill.Module (ModuleName)

data Symbol
    = StringTypeSymbol
    | ModuleSymbol ModuleName
    | DeclSymbol ID
    deriving (Eq, Show)

type SymbolTable = Map String Symbol

defaultSymbolTable :: SymbolTable
defaultSymbolTable = Map.fromList $
    [ ("__String", StringTypeSymbol)
    ]

resolveNamesInModules :: Map ModuleName Module -> Map ID Symbol
resolveNamesInModules ms = mconcat $ map (resolveNamesInModule ms . fst) (Map.toList ms)

resolveNamesInModule :: Map ModuleName Module -> ModuleName -> Map ID Symbol
resolveNamesInModule ms m = runST $ do
    let (Module decls) = ms Map.! m
    st <- newSTRef defaultSymbolTable
    resolvedNames <- newSTRef Map.empty
    forM_ decls $ \decl -> do
        (declST, declResolvedNames) <-
            (flip resolveNamesInDecl decl) <$> readSTRef st
        modifySTRef' st (Map.union declST)
        modifySTRef' resolvedNames (Map.union declResolvedNames)
    readSTRef resolvedNames

resolveNamesInDecl :: SymbolTable -> Decl -> (SymbolTable, Map ID Symbol)
resolveNamesInDecl st decl = case decl of
    ImportDecl _ m ->
        undefined

    SubDecl id name params returnType body ->
        let resolvedNames = runST $ do
                bodyST <- newSTRef (Map.insert name (DeclSymbol id) st)
                resolved <- newSTRef Map.empty
                forM_ params $ \(Parameter paramID paramName paramType) -> do
                    modifySTRef' bodyST (Map.insert paramName (DeclSymbol paramID))
                    modifySTRef' resolved (Map.union (resolveNamesInType st paramType))
                bodyST' <- readSTRef bodyST
                modifySTRef' resolved (Map.union (resolveNamesInExpr bodyST' body))
                readSTRef resolved
         in (Map.singleton name (DeclSymbol id), resolvedNames)

    AliasDecl id name aliasedType ->
        (Map.singleton name (DeclSymbol id), resolveNamesInType st aliasedType)

    StructDecl id name fields ->
        let resolvedNames =
                mconcat $ map (\(Field _ t) -> resolveNamesInType st t) fields
         in (Map.singleton name (DeclSymbol id), resolvedNames)

resolveNamesInExpr :: SymbolTable -> Expr -> Map ID Symbol
resolveNamesInExpr st expr = case expr of
    BlockExpr _ stmts -> runST $ do
        blockST <- newSTRef st
        resolved <- newSTRef Map.empty
        forM_ stmts $ \stmt -> case stmt of
            ExprStmt expr -> do
                st' <- readSTRef blockST
                modifySTRef' resolved (Map.union (resolveNamesInExpr st' expr))
            DeclStmt decl -> do
                (declST, declResolvedNames) <-
                    (flip resolveNamesInDecl decl) <$> readSTRef blockST
                modifySTRef' blockST (Map.union declST)
                modifySTRef' resolved (Map.union declResolvedNames)
        readSTRef resolved

    CallExpr _ callee args ->
        mconcat $ map (resolveNamesInExpr st) (callee : args)

    NameExpr id (UnqualifiedName name) ->
        Map.singleton id (st Map.! name)

    NameExpr id (QualifiedName moduleName name) ->
        error "not implemented"

    StringLiteralExpr _ _ ->
        Map.empty

resolveNamesInType :: SymbolTable -> Type -> Map ID Symbol
resolveNamesInType st type_ = case type_ of
    NamedType id (UnqualifiedName name) ->
        Map.singleton id (st Map.! name)

    NamedType id (QualifiedName moduleName name) ->
        error "not implemented"

    SubType _ paramTypes returnType ->
        mconcat $ map (resolveNamesInType st) (returnType : paramTypes)

    TupleType _ elementTypes ->
        mconcat $ map (resolveNamesInType st) elementTypes
