module Language.Mill.AST.ID
( ID
, TypeID(..)
, DeclID(..)
, ExprID(..)
, newID
) where

import Control.Applicative ((<$>))
import Text.Parsec (ParsecT, getState, modifyState)

class ID a where
    idFromInt :: Int -> a
    idToInt :: a -> Int

newtype TypeID = TypeID Int
                 deriving (Show)

instance ID TypeID where
    idFromInt = TypeID
    idToInt (TypeID id) = id

newtype DeclID = DeclID Int
                 deriving (Show)

instance ID DeclID where
    idFromInt = DeclID
    idToInt (DeclID id) = id

newtype ExprID = ExprID Int
                 deriving (Show)

instance ID ExprID where
    idFromInt = ExprID
    idToInt (ExprID id) = id

instance Eq TypeID where
    (==) = idEq

instance Eq DeclID where
    (==) = idEq

instance Eq ExprID where
    (==) = idEq

idEq :: ID a => a -> a -> Bool
a `idEq` b = idToInt a == (-1) || idToInt b == (-1) || idToInt a == idToInt b

newID :: (Monad m, ID a) => ParsecT s Int m a
newID = do
    id <- idFromInt <$> getState
    modifyState succ
    return id
