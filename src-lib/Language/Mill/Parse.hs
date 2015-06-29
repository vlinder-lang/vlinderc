module Language.Mill.Parse
( name
, parameter
, parameterList
, expr
, blockExpr
, subDecl
) where

import Control.Applicative ((<$>), (<*), (*>))
import Text.Parsec (sepBy, sepBy1, sepEndBy, many)
import Text.Parsec.String (Parser)
import Language.Mill.Lex
import Language.Mill.AST (ModuleName(..), Name(..), Parameter(..), ParameterList, Type(..), Decl(..), Expr(..))

name :: Parser Name
name = do
    parts <- identifier `sepBy1` dot
    return $ case parts of
        [id] -> UnqualifiedName id
        xs -> QualifiedName (ModuleName $ init xs) (last xs)

typeName :: Parser Type
typeName = NamedType <$> name

parameter :: Parser Parameter
parameter = do
    id <- identifier
    colon
    idType <- typeName
    return $ Parameter id idType

parameterList :: Parser ParameterList
parameterList = openingParenthesis *> parameter `sepEndBy` comma <* closingParenthesis

expr :: Parser Expr
expr = blockExpr

blockExpr :: Parser Expr
blockExpr = BlockExpr <$> (openingBrace *> many expr <* closingBrace)

subDecl :: Parser Decl
subDecl = do
    subKeyword
    id <- identifier
    params <- parameterList
    colon
    retType <- typeName
    body <- blockExpr
    return $ SubDecl id params retType body
