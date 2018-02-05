{-# LANGUAGE ParallelListComp #-}

module Interpreter
  ( run )
where

import DataSet
import Expr
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

   mkGroup :: Record -> ([Data], [Record])
   mkGroup r = (listTakeIndices fields r, [listDropIndices fields r])


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
         columns' = [ show (agg f c) | c <- columns | f <- fs ]
      in
         Seq [columns']

   agg AggSum = sum . map (readNum 0)
   agg AggProd = product . map (readNum 1)


doProject :: [ProjExpr] -> DataSet Record -> DataSet Record
doProject pexprs = go where
   go (Group {}) =
      error "Cannot project a group"
   go (Seq rs) =
      Seq $ fmap (recordProject pexprs) rs

--------------------------------------------------------------------------------

recordGet :: Field -> Record -> Data
recordGet i r =
   case List.take 1 $ List.drop i r of
      [x] -> x
      _   -> nullData


recordProject :: [ProjExpr] -> Record -> Record
recordProject pexprs r = map get pexprs
   where
      get (ProjField i) = recordGet i r
      get (ProjValue v) = v


readNum :: Double -> Data -> Double
readNum def x = read x

--------------------------------------------------------------------------------

listTakeIndices :: [Int] -> [a] -> [a]
listTakeIndices is = map snd . filter (\(i,_) -> i `elem` is) . zip [0..]

listDropIndices :: [Int] -> [a] -> [a]
listDropIndices is = map snd . filter (\(i,_) -> not (i `elem` is)) . zip [0..]
