module Language.Mill.Module where

newtype ModuleName = ModuleName [String]
                     deriving (Eq, Ord, Show)