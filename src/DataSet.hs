module DataSet
  ( DataSet(..)
  , isEmptyDataSet
  , module Data
  )
where

import Common
import Data
import Record

--------------------------------------------------------------------------------

data DataSet a
   = Seq [a]
   | Group (Map Record (DataSet a))
   deriving (Show)

instance Functor DataSet where
   fmap f (Seq rs) = Seq (fmap f rs)
   fmap f (Group m) = Group (fmap (fmap f) m)


isEmptyDataSet :: DataSet a -> Bool
isEmptyDataSet = o where
   o (Seq xs) = null xs
   o (Group m) = null m
