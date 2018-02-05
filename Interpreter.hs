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

run :: [Expr] -> DataSet Record -> DataSet Record
run exprs ds = foldl next ds exprs

next :: DataSet Record -> Expr -> DataSet Record
next input = go where
   go Flatten =
      doFlatten input
   go (GroupBy fields exprs) =
      doGroup fields exprs input
   go (Aggregate fields) =
      doAggregate fields input
   go (Project xs) =
      doProject xs input


doFlatten :: DataSet Record -> DataSet Record
doFlatten = Seq . go where
   go (Group m) =
      concatMap (\(h,ds) -> fmap (h <>) $ go ds) $
      Map.toList m
   go (Seq rs) = rs


doGroup :: [Field] -> [Expr] -> DataSet Record -> DataSet Record
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


doAggregate :: [(Field, AggFun)] -> DataSet Record -> DataSet Record
doAggregate fields = go where
   go (Group {}) =
      error "Cannot aggregate a group"
   go (Seq rs) =
      let
         is = map fst fields
         fs = map snd fields
         rs' = [[recordGet i r | i <- is] | r <- rs]
         columns = List.transpose rs'
         columns' = [ numToData (agg f c) | c <- columns | f <- fs ]
      in
         Seq [recordFromList columns']

   agg AggSum = sum . map (readNum 0)
   agg AggProd = product . map (readNum 1)
   agg AggCount = fromIntegral . length


doProject :: [ProjExpr] -> DataSet Record -> DataSet Record
doProject pexprs = go where
   go (Group {}) =
      error "Cannot project a group"
   go (Seq rs) =
      Seq $ fmap (recordProject pexprs) rs

--------------------------------------------------------------------------------
