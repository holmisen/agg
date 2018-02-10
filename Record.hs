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

import Common
import Data
import Expr
import VExpr

import Data.List as List
import Data.Text (pack)
import Text.Printf (printf)

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


recordProject :: [ProjExpr Field] -> Record -> Record
recordProject pexprs r = Record $ [eval r x | ProjExpr x _ <- pexprs]


eval :: Record -> VExpr Int -> Data
eval r = go where
   go (VField i) = recordGet i r
   go (VData d)  = d
   go (VAdd x y) = Data.applyNumOp (+) (go x) (go y)
   go (VSub x y) = Data.applyNumOp (-) (go x) (go y)
   go (VMul x y) = Data.applyNumOp (*) (go x) (go y)
   go (VDiv x y) = Data.applyNumOp (/) (go x) (go y)
   go (VDec n m x) = case go x of
      DataDbl d ->
         format n m d
      txt ->
         maybe txt (format n m) $ dataGetDouble txt

format :: Int -> Int -> Double -> Data
format n m = DataTxt . pack . printf (concat ["%", show n, ".", show m, "f"])


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
