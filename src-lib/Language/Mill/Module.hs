module Language.Mill.Module where

import Data.List (intercalate)

newtype ModuleName = ModuleName [String]
                     deriving (Eq, Ord, Show)

showModuleName :: ModuleName -> String
showModuleName (ModuleName ss) = intercalate "." ss
