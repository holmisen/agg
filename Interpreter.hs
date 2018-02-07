{-# LANGUAGE ParallelListComp #-}

module Interpreter
  ( run )
where

import DataSet
import Expr
import Record
import Types

import qualified Data.List as List
import qualified Data.Map as Map

--------------------------------------------------------------------------------

run :: [Expr Field] -> DataSet Record -> DataSet Record
run exprs ds = foldl next ds exprs

next :: DataSet Record -> Expr Field -> DataSet Record
next input = go where
   go Flatten =
      doFlatten input
   go (GroupBy fields exprs) =
      doGroup fields exprs input
   go (Aggregate fields) =
      doAggregate fields input
   go (Project xs) =
      doProject xs input
   go (SortBy fields) =
      doSortBy fields input


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

   agg AggSum = DataDbl . sum . map (dataToDouble 0)
   agg AggProd = DataDbl . product . map (dataToDouble 1)
   agg AggCount = DataDbl . fromIntegral . length


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
