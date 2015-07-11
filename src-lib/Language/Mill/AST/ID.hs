module Language.Mill.AST.ID
( ID(..)
, newID
) where

import Control.Applicative ((<$>))
import Text.Parsec (ParsecT, getState, modifyState)

newtype ID = ID Int
             deriving (Ord, Show)

idFromInt :: Int -> ID
idFromInt = ID

idToInt :: ID -> Int
idToInt (ID id) = id

instance Eq ID where
    a == b = idToInt a == (-1) || idToInt b == (-1) || idToInt a == idToInt b

newID :: Monad m => ParsecT s Int m ID
newID = do
    id <- idFromInt <$> getState
    modifyState succ
    return id
