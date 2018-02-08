{-# LANGUAGE ParallelListComp #-}

module Interpreter
  ( run )
where

import Common
import DataSet
import Expr
import Record

import qualified Data.List as List
import qualified Data.Map as Map

--------------------------------------------------------------------------------

run :: [Expr Field] -> DataSet Record -> DataSet Record
run exprs ds = foldl next ds exprs

next :: DataSet Record -> Expr Field -> DataSet Record
next = flip go where
   go Flatten =
      doFlatten
   go (GroupBy fields exprs) =
      doGroup fields exprs
   go (Aggregate fields) =
      doAggregate fields
   go (Project xs) =
      doProject xs
   go (SortBy fields) =
      doSortBy fields
   go (TakeN n) =
      doTakeN n
   go (CrossJoin exprs) =
      doCrossJoin exprs


doFlatten :: DataSet Record -> DataSet Record
doFlatten = Seq . go where
   go (Group m) =
      concatMap (\(h,ds) -> fmap (h <>) $ go ds)
      $ filter (\(_,ds) -> not $ isEmptyDataSet ds)
      $ Map.toList m
   go (Seq rs) = rs


doGroup :: [Field] -> [Expr Field] -> DataSet Record -> DataSet Record
doGroup fields exprs = go where
   go (Group {}) =
      error "Cannot group a group"
   go (Seq rs) =
      Group
      $ fmap (run exprs . Seq)
      $ Map.fromListWith (<>)
      $ map mkGroup
      $ rs

   mkGroup :: Record -> (Record, [Record])
   mkGroup r = (recordTakeSubset fields r, [recordDropSubset fields r])


doAggregate :: [(AggFun,Field)] -> DataSet Record -> DataSet Record
doAggregate fields = go where
   go (Group {}) =
      error "Cannot aggregate a group"
   go (Seq rs) =
      let
         is = map snd fields
         fs = map fst fields
         rs' = [[recordGet i r | i <- is] | r <- rs]
         columns = List.transpose rs'
         columns' = [ agg f c | c <- columns | f <- fs ]
      in
         Seq [recordFromList columns']

   agg AggSum = DataDbl . sum . getNumbers
   agg AggProd = DataDbl . product . getNumbers
   agg AggCount = DataDbl . fromIntegral . length
   agg AggMax = \xs ->
      case getNumbers xs of
         [] -> nullData
         xs -> DataDbl $ List.maximum xs
   agg AggMin = \xs ->
      case getNumbers xs of
         [] -> nullData
         xs -> DataDbl $ List.minimum xs

   getNumbers = catMaybes . map dataGetDouble


doProject :: [ProjExpr Field] -> DataSet Record -> DataSet Record
doProject pexprs = go where
   go (Group {}) =
      error "Cannot project a group"
   go (Seq rs) =
      Seq $ fmap (recordProject pexprs) rs


doSortBy :: [(Order, Field)] -> DataSet Record -> DataSet Record
doSortBy fields = go where
   go (Group {}) =
      error "Cannot sort a group"
   go (Seq rs) =
      Seq $ List.sortBy (compareRecords fields) rs


doTakeN :: Int -> DataSet Record -> DataSet Record
doTakeN n = go where
   go (Group m) =
      Group $ Map.take n m
   go (Seq rs) =
      Seq $ List.take n rs


doCrossJoin :: [Expr Field] -> DataSet Record -> DataSet Record
doCrossJoin exprs = go where
   go (Group {}) =
      error "Cannot cross join group"
   go (Seq rs) =
      Seq [ r <> r' | r <- rs, r' <- rs' ]
      where
         (Seq rs') = doFlatten $ run exprs (Seq rs)

--------------------------------------------------------------------------------

compareRecords :: [(Order, Field)] -> Record -> Record -> Ordering
compareRecords fields r1 r2 =
   mconcat
   [ (recordGet i r1) `cmp` (recordGet i r2)
   | (o,i) <- fields
   , let cmp = case o of
            Asc  -> compare
            Desc -> flip compare
   ]
