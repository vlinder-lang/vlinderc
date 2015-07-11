module Language.Mill.Name where

import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.Morph (lift)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT)
import Data.Map (Map)
import Data.Set (Set)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Monoid (mconcat)
import Language.Mill.AST
import Language.Mill.AST.ID (ID)
import Language.Mill.Module (ModuleName(..), showModuleName)

type NameResolving a = Either String a

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

resolveNamesInModule :: Map ModuleName Module -> ModuleName -> NameResolving (Map ID Symbol)
resolveNamesInModule ms m = runST $ runEitherT $ do
    let (Module decls) = ms Map.! m
    st <- lift $ newSTRef defaultSymbolTable
    resolvedNames <- lift $ newSTRef Map.empty
    forM_ decls $ \decl -> do
        (declST, declResolvedNames) <- do
            st' <- lift $ readSTRef st
            hoistEither $ resolveNamesInDecl ms st' decl
        lift $ modifySTRef' st (Map.union declST)
        lift $ modifySTRef' resolvedNames (Map.union declResolvedNames)
    lift $ readSTRef resolvedNames

resolveNamesInDecl :: Map ModuleName Module -> SymbolTable -> Decl -> NameResolving (SymbolTable, Map ID Symbol)
resolveNamesInDecl ms st decl = case decl of
    ImportDecl _ m@(ModuleName mp) -> do
        resolved <- resolveNamesInModule ms m
        return (Map.singleton (last mp) (ModuleSymbol m), resolved)

    SubDecl id name params returnType body -> do
        resolvedNames <- runST $ runEitherT $ do
            bodyST <- lift $ newSTRef (Map.insert name (DeclSymbol id) st)
            resolved <- lift $ newSTRef Map.empty

            forM_ params $ \(Parameter paramID paramName paramType) -> do
                lift $ modifySTRef' bodyST (Map.insert paramName (DeclSymbol paramID))
                resolvedInType <- hoistEither $ resolveNamesInType ms st paramType
                lift $ modifySTRef' resolved (Map.union resolvedInType)

            bodyST' <- lift $ readSTRef bodyST
            resolvedInBody <- hoistEither $ resolveNamesInExpr ms bodyST' body
            lift $ modifySTRef' resolved (Map.union resolvedInBody)

            lift $ readSTRef resolved
        return (Map.singleton name (DeclSymbol id), resolvedNames)

    AliasDecl id name aliasedType -> do
        resolved <- resolveNamesInType ms st aliasedType
        return (Map.singleton name (DeclSymbol id), resolved)

    StructDecl id name fields -> do
        resolved <- mconcat <$> mapM (\(Field _ t) -> resolveNamesInType ms st t) fields
        return (Map.singleton name (DeclSymbol id), resolved)

resolveNamesInExpr :: Map ModuleName Module -> SymbolTable -> Expr -> NameResolving (Map ID Symbol)
resolveNamesInExpr ms st expr = case expr of
    BlockExpr _ stmts -> runST $ runEitherT $ do
        blockST <- lift $ newSTRef st
        resolved <- lift $ newSTRef Map.empty
        forM_ stmts $ \stmt -> case stmt of
            ExprStmt expr -> do
                st' <- lift $ readSTRef blockST
                resolvedInExpr <- hoistEither $ resolveNamesInExpr ms st' expr
                lift $ modifySTRef' resolved (Map.union resolvedInExpr)
            DeclStmt decl -> do
                st' <- lift $ readSTRef blockST
                (declST, declResolvedNames) <- hoistEither $ resolveNamesInDecl ms st' decl
                lift $ modifySTRef' blockST (Map.union declST)
                lift $ modifySTRef' resolved (Map.union declResolvedNames)
        lift $ readSTRef resolved

    CallExpr _ callee args ->
        mconcat <$> mapM (resolveNamesInExpr ms st) (callee : args)

    NameExpr id name ->
        lookupName ms st id name

    StringLiteralExpr _ _ ->
        return Map.empty

resolveNamesInType :: Map ModuleName Module -> SymbolTable -> Type -> NameResolving (Map ID Symbol)
resolveNamesInType ms st type_ = case type_ of
    NamedType id name ->
        lookupName ms st id name

    SubType _ paramTypes returnType ->
        mconcat <$> mapM (resolveNamesInType ms st) (returnType : paramTypes)

    TupleType _ elementTypes ->
        mconcat <$> mapM (resolveNamesInType ms st) elementTypes

lookupName :: Map ModuleName Module -> SymbolTable -> ID -> Name -> NameResolving (Map ID Symbol)
lookupName ms st id name = case name of
    UnqualifiedName name ->
        case name `Map.lookup` st of
            Just symbol -> return $ Map.singleton id symbol
            Nothing -> failNotInScope name

    QualifiedName unqualifiedModuleName name ->
        case unqualifiedModuleName `Map.lookup` st of
            Just (ModuleSymbol moduleName) ->
                case idForDeclInModule (ms Map.! moduleName) name of
                    Just declID -> return $ Map.singleton id (DeclSymbol declID)
                    Nothing -> failNotExported moduleName name
            Just _ -> failNotAModule unqualifiedModuleName
            Nothing -> failNotInScope unqualifiedModuleName

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

failNotInScope :: String -> NameResolving a
failNotInScope name = failWithMessage $ "name '" ++ name ++ "' is not in scope"

failNotExported :: ModuleName -> String -> NameResolving a
failNotExported moduleName name = failWithMessage $ "module '" ++ showModuleName moduleName ++ "' does not export name '" ++ name ++ "'"

failNotAModule :: String -> NameResolving a
failNotAModule name = failWithMessage $ "name '" ++ name ++ "' does not refer to an imported module"

failWithMessage :: String -> NameResolving a
failWithMessage = Left
