module Language.Mill.Parse where

import Control.Applicative ((<$>), (<|>), (<*), (<*>), (*>))
import Data.List (foldl')
import Text.Parsec (eof, sepBy, sepBy1, try, sepEndBy, many)
import Text.Parsec.String (Parser)
import Language.Mill.Lex
import Language.Mill.AST

module_ :: Parser Module
module_ = Module <$> many decl <* eof

name :: Parser Name
name = do
    parts <- identifier `sepBy1` dot
    return $ case parts of
        [id] -> UnqualifiedName id
        xs -> QualifiedName (ModuleName $ init xs) (last xs)

type_ :: Parser Type
type_ = namedType <|> try subType <|> tupleType

namedType :: Parser Type
namedType = NamedType <$> name

subType :: Parser Type
subType = do
    parameterTypes <- (openingParenthesis *> type_ `sepEndBy` comma <* closingParenthesis)
    fatArrow
    returnType <- type_
    return $ SubType parameterTypes returnType

tupleType :: Parser Type
tupleType = TupleType <$> (openingParenthesis *> type_ `sepEndBy` comma <* closingParenthesis)

parameter :: Parser Parameter
parameter = do
    id <- identifier
    colon
    idType <- type_
    return $ Parameter id idType

parameterList :: Parser ParameterList
parameterList = openingParenthesis *> parameter `sepEndBy` comma <* closingParenthesis

expr :: Parser Expr
expr = callExpr

callExpr :: Parser Expr
callExpr = do
    callee <- primaryExpr
    argumentLists <- many argumentList
    return $ foldl' CallExpr callee argumentLists
    where
        argumentList :: Parser [Expr]
        argumentList = openingParenthesis *> expr `sepEndBy` comma <* closingParenthesis

primaryExpr :: Parser Expr
primaryExpr = nameExpr <|> stringLiteralExpr <|> blockExpr

nameExpr :: Parser Expr
nameExpr = NameExpr <$> name

stringLiteralExpr :: Parser Expr
stringLiteralExpr = StringLiteralExpr <$> stringLiteral

blockExpr :: Parser Expr
blockExpr = BlockExpr <$> (openingBrace *> many stmt <* closingBrace)

stmt :: Parser Stmt
stmt = (ExprStmt <$> expr) <|> (DeclStmt <$> decl)

decl :: Parser Decl
decl = aliasDecl <|> importDecl <|> try structDecl <|> subDecl

aliasDecl :: Parser Decl
aliasDecl = do
    aliasKeyword
    name <- identifier
    equalsSign
    original <- type_
    return $ AliasDecl name original

importDecl :: Parser Decl
importDecl = ImportDecl . ModuleName <$> (importKeyword *> identifier `sepBy1` dot)

structDecl :: Parser Decl
structDecl = do
    structKeyword
    name <- identifier
    openingBrace
    fields <- many field
    closingBrace
    return $ StructDecl name fields
    where
        field :: Parser Field
        field = Field <$> identifier <*> (colon *> type_)

subDecl :: Parser Decl
subDecl = do
    subKeyword
    id <- identifier
    params <- parameterList
    colon
    retType <- type_
    body <- blockExpr
    return $ SubDecl id params retType body
