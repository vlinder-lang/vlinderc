module Language.Mill.LexSpec where

import Control.Applicative ((<*))
import Control.Monad (forM_)
import Data.Char (toUpper)
import Data.Either (rights)
import Language.Mill.Lex (aliasKeyword, identifier, importKeyword, subKeyword, stringLiteral)
import Test.Hspec (describe, it, shouldBe, Spec)
import Text.Parsec (eof, parse)

-- TODO move this
ucFirst :: String -> String
ucFirst [] = []
ucFirst (x : xs) = toUpper x : xs

spec :: Spec
spec = do
    let keywords = [("alias", aliasKeyword), ("import", importKeyword), ("sub", subKeyword)]

    describe "Language.Mill.Lex.identifier" $ do
        it "lexes one-character identifiers" $ do
            forM_ ["_", "a", "A"] $ \id -> do
                rights [parse (identifier <* eof) "" id] `shouldBe` [id]

        it "lexes multicharacter identifiers" $ do
            forM_ ["__", "a_b", "Ab", "aB", "abC_", "abc9023"] $ \id -> do
                rights [parse (identifier <* eof) "" id] `shouldBe` [id]

        it "does not lex keywords" $ do
            forM_ (map fst keywords) $ \id -> do
                rights [parse (identifier <* eof) "" id] `shouldBe` []

    forM_ keywords $ \(keyword, keywordParser) -> do
        describe ("Language.Mill.Lex.keyword" ++ ucFirst keyword) $ do
            it ("lexes '" ++ keyword ++ "'") $ do
                rights [parse (keywordParser <* eof) "" keyword] `shouldBe` [()]

            it ("does not lex '" ++ keyword ++ "x'") $ do
                rights [parse (keywordParser <* eof) "" (keyword ++ "x")] `shouldBe` []

            it "does not lex something weird" $ do
                rights [parse (keywordParser <* eof) "" "bullshit"] `shouldBe` []

    describe "Language.Mill.Lex.stringLiteral" $ do
        it "lexes string literals" $ do
            forM_ ["", "a", "A B C"] $ \str -> do
                rights [parse (stringLiteral <* eof) "" ("\"" ++ str ++ "\"")] `shouldBe` [str]
