module Language.Mill.Lex
( identifier

, stringLiteral

, aliasKeyword
, importKeyword
, subKeyword

, closingBrace
, closingParenthesis
, openingBrace
, openingParenthesis

, colon
, comma
, dot
, fatArrow
, equalsSign
) where

import Control.Applicative ((<$>), (<*), (<*>), (*>), (<|>))
import Control.Monad (void, when)
import Text.Parsec (char, many, noneOf, notFollowedBy, oneOf, string, sepBy)
import Text.Parsec.String (Parser)

lexeme :: Parser a -> Parser a
lexeme p = many space *> p <* many space

space :: Parser ()
space = void $ oneOf [' ', '\n']

identifier :: Parser String
identifier = lexeme $ do
    id <- (:) <$> identifierHead <*> identifierTail
    when (id `elem` ["alias", "import", "sub"]) $ fail "identifier"
    return id

identifierHead :: Parser Char
identifierHead = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

identifierTail :: Parser String
identifierTail = many (identifierHead <|> oneOf ['0'..'9'])

stringLiteral :: Parser String
stringLiteral = char '"' *> many (noneOf ['"']) <* char '"'

keyword :: String -> Parser ()
keyword kw = lexeme $ string kw >> notFollowedBy identifierTail
aliasKeyword = keyword "alias"
importKeyword = keyword "import"
subKeyword = keyword "sub"

punctuation :: String -> Parser ()
punctuation p = void . lexeme $ string p
closingBrace = punctuation "}"
closingParenthesis = punctuation ")"
colon = punctuation ":"
comma = punctuation ","
dot = punctuation "."
openingBrace = punctuation "{"
openingParenthesis = punctuation "("
fatArrow = punctuation "=>"
equalsSign = punctuation "=" >> notFollowedBy (char '>')
