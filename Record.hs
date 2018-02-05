module Record
  ( Record
  , emptyRecord

  , recordGet
  , recordProject

  , recordTakeSubset
  , recordDropSubset

  , recordFromList
  , recordToList
  )
where

import Expr
import Types

import Data.List as List

--------------------------------------------------------------------------------

newtype Record = Record [Data] deriving (Eq,Ord,Show)

instance Monoid Record where
   mempty = Record mempty
   mappend (Record x) (Record y) = Record (mappend x y)


emptyRecord :: Record
emptyRecord = Record []


recordGet :: Int -> Record -> Data
recordGet i (Record xs) =
   case List.take 1 $ List.drop i xs of
      [x] -> x
      _   -> nullData


recordProject :: [ProjExpr] -> Record -> Record
recordProject pexprs r = Record $ map get pexprs
   where
      get (ProjField i) = recordGet i r
      get (ProjValue v) = v


recordTakeSubset :: [Int] -> Record -> Record
recordTakeSubset is (Record xs) = Record $ listTakeIndices is xs


recordDropSubset :: [Int] -> Record -> Record
recordDropSubset is (Record xs) = Record $ listDropIndices is xs


recordToList :: Record -> [Data]
recordToList (Record xs) = xs


recordFromList :: [Data] -> Record
recordFromList = Record

--------------------------------------------------------------------------------

listTakeIndices :: [Int] -> [a] -> [a]
listTakeIndices is = map snd . filter (\(i,_) -> i `elem` is) . zip [0..]

listDropIndices :: [Int] -> [a] -> [a]
listDropIndices is = map snd . filter (\(i,_) -> not (i `elem` is)) . zip [0..]
