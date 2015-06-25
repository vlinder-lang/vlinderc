module Language.Mill.LexSpec where

import Control.Applicative ((<*))
import Control.Monad (forM_)
import Data.Char (toUpper)
import Data.Either (rights)
import Language.Mill.Lex (identifier, importKeyword, subKeyword)
import Test.Hspec (describe, it, shouldBe, Spec)
import Text.Parsec (eof, parse)

spec :: Spec
spec = do
    let keywords = [("import", importKeyword), ("sub", subKeyword)]

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

    forM_ keywords $ \(keyword @ (k : ks), keywordParser) -> do
        describe ("Language.Mill.Lex.keyword" ++ (toUpper k : ks)) $ do
            it ("lexes '" ++ keyword ++ "'") $ do
                rights [parse (keywordParser <* eof) "" keyword] `shouldBe` [()]

            it ("does not lex '" ++ keyword ++ "x'") $ do
                rights [parse (keywordParser <* eof) "" (keyword ++ "x")] `shouldBe` []

            it "does not lex something weird" $ do
                rights [parse (keywordParser <* eof) "" "bullshit"] `shouldBe` []
