module Language.Mill.Parse where

import Control.Applicative ((<$>), (<|>), (<*), (*>))
import Text.Parsec (sepBy, sepBy1, sepEndBy, many)
import Text.Parsec.String (Parser)
import Language.Mill.Lex
import Language.Mill.AST

name :: Parser Name
name = do
    parts <- identifier `sepBy1` dot
    return $ case parts of
        [id] -> UnqualifiedName id
        xs -> QualifiedName (ModuleName $ init xs) (last xs)

type_ :: Parser Type
type_ = namedType <|> subType

namedType :: Parser Type
namedType = NamedType <$> name

subType :: Parser Type
subType = do
    parameterTypes <- (openingParenthesis *> type_ `sepEndBy` comma <* closingParenthesis)
    fatArrow
    returnType <- type_
    return $ SubType parameterTypes returnType

parameter :: Parser Parameter
parameter = do
    id <- identifier
    colon
    idType <- type_
    return $ Parameter id idType

parameterList :: Parser ParameterList
parameterList = openingParenthesis *> parameter `sepEndBy` comma <* closingParenthesis

expr :: Parser Expr
expr = blockExpr

blockExpr :: Parser Expr
blockExpr = BlockExpr <$> (openingBrace *> many stmt <* closingBrace)

stmt :: Parser Stmt
stmt = (ExprStmt <$> expr) <|> (DeclStmt <$> decl)

decl :: Parser Decl
decl = subDecl

subDecl :: Parser Decl
subDecl = do
    subKeyword
    id <- identifier
    params <- parameterList
    colon
    retType <- type_
    body <- blockExpr
    return $ SubDecl id params retType body
