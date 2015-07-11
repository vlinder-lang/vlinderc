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
import Language.Mill.Module (ModuleName(..))

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

resolveNamesInModule :: Map ModuleName Module -> ModuleName -> Map ID Symbol
resolveNamesInModule ms m = runST $ do
    let (Module decls) = ms Map.! m
    st <- newSTRef defaultSymbolTable
    resolvedNames <- newSTRef Map.empty
    forM_ decls $ \decl -> do
        (declST, declResolvedNames) <-
            (\st' -> resolveNamesInDecl ms st' decl) <$> readSTRef st
        modifySTRef' st (Map.union declST)
        modifySTRef' resolvedNames (Map.union declResolvedNames)
    readSTRef resolvedNames

resolveNamesInDecl :: Map ModuleName Module -> SymbolTable -> Decl -> (SymbolTable, Map ID Symbol)
resolveNamesInDecl ms st decl = case decl of
    ImportDecl _ m@(ModuleName mp) ->
        let resolved = resolveNamesInModule ms m
         in (Map.singleton (last mp) (ModuleSymbol m), resolved)

    SubDecl id name params returnType body ->
        let resolvedNames = runST $ do
                bodyST <- newSTRef (Map.insert name (DeclSymbol id) st)
                resolved <- newSTRef Map.empty
                forM_ params $ \(Parameter paramID paramName paramType) -> do
                    modifySTRef' bodyST (Map.insert paramName (DeclSymbol paramID))
                    modifySTRef' resolved (Map.union (resolveNamesInType ms st paramType))
                bodyST' <- readSTRef bodyST
                modifySTRef' resolved (Map.union (resolveNamesInExpr ms bodyST' body))
                readSTRef resolved
         in (Map.singleton name (DeclSymbol id), resolvedNames)

    AliasDecl id name aliasedType ->
        (Map.singleton name (DeclSymbol id), resolveNamesInType ms st aliasedType)

    StructDecl id name fields ->
        let resolvedNames =
                mconcat $ map (\(Field _ t) -> resolveNamesInType ms st t) fields
         in (Map.singleton name (DeclSymbol id), resolvedNames)

resolveNamesInExpr :: Map ModuleName Module -> SymbolTable -> Expr -> Map ID Symbol
resolveNamesInExpr ms st expr = case expr of
    BlockExpr _ stmts -> runST $ do
        blockST <- newSTRef st
        resolved <- newSTRef Map.empty
        forM_ stmts $ \stmt -> case stmt of
            ExprStmt expr -> do
                st' <- readSTRef blockST
                modifySTRef' resolved (Map.union (resolveNamesInExpr ms st' expr))
            DeclStmt decl -> do
                (declST, declResolvedNames) <-
                    (\st' -> resolveNamesInDecl ms st' decl) <$> readSTRef blockST
                modifySTRef' blockST (Map.union declST)
                modifySTRef' resolved (Map.union declResolvedNames)
        readSTRef resolved

    CallExpr _ callee args ->
        mconcat $ map (resolveNamesInExpr ms st) (callee : args)

    NameExpr id (UnqualifiedName name) ->
        Map.singleton id (st Map.! name)

    NameExpr id (QualifiedName moduleName name) ->
        error "not implemented"

    StringLiteralExpr _ _ ->
        Map.empty

resolveNamesInType :: Map ModuleName Module -> SymbolTable -> Type -> Map ID Symbol
resolveNamesInType ms st type_ = case type_ of
    NamedType id (UnqualifiedName name) ->
        Map.singleton id (st Map.! name)

    NamedType id (QualifiedName unqualifiedModuleName name) ->
        let (ModuleSymbol moduleName) = st Map.! unqualifiedModuleName
            module_ = ms Map.! moduleName
            Just declID = idForDeclInModule module_ name
         in Map.singleton id (DeclSymbol declID)

    SubType _ paramTypes returnType ->
        mconcat $ map (resolveNamesInType ms st) (returnType : paramTypes)

    TupleType _ elementTypes ->
        mconcat $ map (resolveNamesInType ms st) elementTypes

idForDeclInModule :: Module -> String -> Maybe ID
idForDeclInModule (Module decls) name =
    case filter ((Just name ==) . declName) decls of
        [] -> Nothing
        (decl : _) -> Just (declID decl)
    where declName :: Decl -> Maybe String
          declName (ImportDecl _ _) = Nothing
          declName (SubDecl _ n _ _ _) = Just n
          declName (ForeignSubDecl _ _ _ n _ _) = Just n
          declName (AliasDecl _ n _) = Just n
          declName (StructDecl _ n _) = Just n

          declID :: Decl -> ID
          declID (ImportDecl id _) = id
          declID (SubDecl id _ _ _ _) = id
          declID (ForeignSubDecl id _ _ _ _ _) = id
          declID (AliasDecl id _ _) = id
          declID (StructDecl id _ _) = id