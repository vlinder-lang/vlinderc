module Language.Mill.Parse where

import Control.Applicative ((<$>), (<|>), (<*), (<*>), (*>))
import Control.Monad (foldM)
import Language.Mill.AST
import Language.Mill.AST.ID (newID)
import Language.Mill.Lex
import Language.Mill.Module (ModuleName(..))
import Text.Parsec (eof, sepBy, sepBy1, try, sepEndBy, many, optionMaybe)

module_ :: Parser Module
module_ = Module <$> many decl <* eof

name :: Parser Name
name = do
    part1 <- identifier
    part2 <- optionMaybe (dot *> identifier)
    return $ case part2 of
        Nothing -> UnqualifiedName part1
        Just id -> QualifiedName part1 id

type_ :: Parser Type
type_ = namedType <|> try subType <|> tupleType

namedType :: Parser Type
namedType = NamedType <$> newID <*> name

subType :: Parser Type
subType = do
    id <- newID
    parameterTypes <- (openingParenthesis *> type_ `sepEndBy` comma <* closingParenthesis)
    fatArrow
    returnType <- type_
    return $ SubType id parameterTypes returnType

tupleType :: Parser Type
tupleType = TupleType <$> newID <*> (openingParenthesis *> type_ `sepEndBy` comma <* closingParenthesis)

parameter :: Parser Parameter
parameter = do
    id <- newID
    name <- identifier
    colon
    paramType <- type_
    return $ Parameter id name paramType

parameterList :: Parser ParameterList
parameterList = openingParenthesis *> parameter `sepEndBy` comma <* closingParenthesis

expr :: Parser Expr
expr = callExpr

callExpr :: Parser Expr
callExpr = do
    callee <- primaryExpr
    argumentLists <- many argumentList
    foldM (\a b -> (\id -> CallExpr id a b) <$> newID) callee argumentLists
    where
        argumentList :: Parser [Expr]
        argumentList = openingParenthesis *> expr `sepEndBy` comma <* closingParenthesis

primaryExpr :: Parser Expr
primaryExpr = try structLiteralExpr <|> nameExpr <|> stringLiteralExpr <|> blockExpr

nameExpr :: Parser Expr
nameExpr = NameExpr <$> newID <*> name

stringLiteralExpr :: Parser Expr
stringLiteralExpr = StringLiteralExpr <$> newID <*> stringLiteral

blockExpr :: Parser Expr
blockExpr = BlockExpr <$> newID <*> (openingBrace *> many stmt <* closingBrace)

structLiteralExpr :: Parser Expr
structLiteralExpr = StructLiteralExpr <$> type_ <*> (openingBrace *> fieldValue `sepBy` comma <* closingBrace)
    where
        fieldValue :: Parser FieldValue
        fieldValue = FieldValue <$> identifier <*> (colon *> expr)

stmt :: Parser Stmt
stmt = (ExprStmt <$> expr) <|> (DeclStmt <$> decl)

decl :: Parser Decl
decl = aliasDecl <|> importDecl <|> try structDecl <|> subDecl <|> foreignSubDecl

aliasDecl :: Parser Decl
aliasDecl = do
    id <- newID
    aliasKeyword
    name <- identifier
    equalsSign
    original <- type_
    return $ AliasDecl id name original

importDecl :: Parser Decl
importDecl = do
    id <- newID
    moduleName <- ModuleName <$> (importKeyword *> identifier `sepBy1` dot)
    return $ ImportDecl id moduleName

structDecl :: Parser Decl
structDecl = do
    id <- newID
    structKeyword
    name <- identifier
    openingBrace
    fields <- many field
    closingBrace
    return $ StructDecl id name fields
    where
        field :: Parser Field
        field = Field <$> identifier <*> (colon *> type_)

subDecl :: Parser Decl
subDecl = do
    id <- newID
    subKeyword
    name <- identifier
    params <- parameterList
    colon
    retType <- type_
    body <- blockExpr
    return $ SubDecl id name params retType body

foreignSubDecl :: Parser Decl
foreignSubDecl = do
    id <- newID
    foreignKeyword
    library <- ForeignLibrary <$> stringLiteral
    subKeyword
    cconv <- CallingConvention <$> name
    name <- identifier
    params <- parameterList
    colon
    retType <- type_
    return $ ForeignSubDecl id library cconv name params retType
