module Language.Mill.LexSpec where

import Control.Applicative ((<*))
import Control.Monad (forM_)
import Data.Char (toUpper)
import Data.Either (rights)
import Language.Mill.Lex
import Test.Hspec (describe, it, shouldBe, Spec)
import Text.Parsec (eof, runParser)

-- TODO move this
ucFirst :: String -> String
ucFirst [] = []
ucFirst (x : xs) = toUpper x : xs

spec :: Spec
spec = do
    let keywords = [("alias", aliasKeyword), ("foreign", foreignKeyword), ("import", importKeyword), ("struct", structKeyword), ("sub", subKeyword)]

    describe "Language.Mill.Lex.identifier" $ do
        it "lexes one-character identifiers" $ do
            forM_ ["_", "a", "A"] $ \id -> do
                rights [runParser (identifier <* eof) 0 "" id] `shouldBe` [id]

        it "lexes multicharacter identifiers" $ do
            forM_ ["__", "a_b", "Ab", "aB", "abC_", "abc9023"] $ \id -> do
                rights [runParser (identifier <* eof) 0 "" id] `shouldBe` [id]

        it "does not lex keywords" $ do
            forM_ (map fst keywords) $ \id -> do
                rights [runParser (identifier <* eof) 0 "" id] `shouldBe` []

    forM_ keywords $ \(keyword, keywordParser) -> do
        describe ("Language.Mill.Lex.keyword" ++ ucFirst keyword) $ do
            it ("lexes '" ++ keyword ++ "'") $ do
                rights [runParser (keywordParser <* eof) 0 "" keyword] `shouldBe` [()]

            it ("does not lex '" ++ keyword ++ "x'") $ do
                rights [runParser (keywordParser <* eof) 0 "" (keyword ++ "x")] `shouldBe` []

            it "does not lex something weird" $ do
                rights [runParser (keywordParser <* eof) 0 "" "bullshit"] `shouldBe` []

    describe "Language.Mill.Lex.stringLiteral" $ do
        it "lexes string literals" $ do
            forM_ ["", "a", "A B C"] $ \str -> do
                rights [runParser (stringLiteral <* eof) 0 "" ("\"" ++ str ++ "\"")] `shouldBe` [str]
