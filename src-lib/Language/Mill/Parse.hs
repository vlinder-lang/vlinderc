module Language.Mill.Parse
( parameter
, parameterList
, blockStmt
, subDecl
) where

import Control.Applicative ((<*), (*>))
import Text.Parsec (sepBy, sepEndBy)
import Text.Parsec.String (Parser)
import Language.Mill.Lex (identifier, comma, colon, openingBrace, closingBrace, openingParenthesis, closingParenthesis, subKeyword)
import Language.Mill.AST (Name(..), Parameter(..), ParameterList, Type(..), Decl(..), Expr(..))

typeName :: Parser Type
typeName = do
  typeId <- identifier
  -- currently, hardcode UnqualifiedName
  return $ NamedType (UnqualifiedName typeId)

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
