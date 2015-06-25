module Language.Mill.LexSpec where

import Control.Monad (forM_)
import Data.Char (toUpper)
import Data.Either (rights)
import Language.Mill.Lex (identifier, importKeyword, subKeyword)
import Test.Hspec (describe, it, shouldBe, Spec)
import Text.Parsec (parse)

spec :: Spec
spec = do
    let keywords = [("import", importKeyword), ("sub", subKeyword)]

    describe "Language.Mill.Lex.identifier" $ do
        it "lexes one-character identifiers" $ do
            forM_ ["_", "a", "A"] $ \id -> do
                rights [parse identifier "" id] `shouldBe` [id]

        it "does not lex keywords" $ do
            forM_ (map fst keywords) $ \id -> do
                rights [parse identifier "" id] `shouldBe` []

    forM_ keywords $ \(keyword @ (k : ks), keywordParser) -> do
        describe ("Language.Mill.Lex.keyword" ++ (toUpper k : ks)) $ do
            it ("lexes '" ++ keyword ++ "'") $ do
                rights [parse keywordParser "" keyword] `shouldBe` [()]

            it ("does not lax '" ++ keyword ++ "x'") $ do
                rights [parse keywordParser "" (keyword ++ "x")] `shouldBe` []

            it "does not lex something weird" $ do
                rights [parse keywordParser "" "bullshit"] `shouldBe` []
