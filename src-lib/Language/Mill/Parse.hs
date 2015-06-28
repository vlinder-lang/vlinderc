module Language.Mill.Parse
( name
, parameter
, parameterList
, blockStmt
, subDecl
) where

import Control.Applicative ((<*), (*>))
import Text.Parsec (sepBy, sepBy1, sepEndBy)
import Text.Parsec.String (Parser)
import Language.Mill.Lex (identifier, colon, comma, dot, openingBrace, closingBrace, openingParenthesis, closingParenthesis, subKeyword)
import Language.Mill.AST (ModuleName(..), Name(..), Parameter(..), ParameterList, Type(..), Decl(..), Expr(..))

name :: Parser Name
name = do
  parts <- sepBy1 identifier dot
  return $ case parts of
    [id] -> UnqualifiedName id 
    xs -> QualifiedName (ModuleName $ init xs) (last xs)

typeName :: Parser Type
typeName = do
  typeId <- name
  return $ NamedType typeId

parameter :: Parser Parameter
parameter = do
  id <- identifier
  colon
  idType <- typeName
  return $ Parameter id idType

parameterList :: Parser ParameterList
parameterList = openingParenthesis *> sepEndBy parameter comma <* closingParenthesis

-- dummy block for now
blockStmt :: Parser Expr
blockStmt = do
  openingBrace
  closingBrace
  return $ BlockExpr []

subDecl :: Parser Decl
subDecl = do
  subKeyword
  id <- identifier
  args <- parameterList
  colon
  retType <- typeName
  block <- blockStmt
  return $ SubDecl id args retType block
