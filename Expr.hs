{-# LANGUAGE DeriveFunctor #-}

module Expr where

import Data
import Types

import Data.List as List

--------------------------------------------------------------------------------

type Field = Int

newtype FieldName = FieldName String
   deriving (Eq, Ord, Show)

data AggFun = AggSum | AggProd | AggCount
   deriving (Eq, Ord, Show)

data ProjExpr f = ProjField f | ProjValue Data f
   deriving (Eq, Functor, Show)

-- | f is the type of field identifiers
data Expr f
   = GroupBy [f] [Expr f]
   | Flatten
   | Project [ProjExpr f]
   | Aggregate [(AggFun, f)]
   deriving (Eq, Show)

--------------------------------------------------------------------------------

getFieldIndex :: [FieldName] -> FieldName -> Int
getFieldIndex ns n@(FieldName x) =
   fromMaybe (error $ "No such name `" ++ x ++ "`") (List.elemIndex n ns)


toIndexedFields :: [FieldName] -> [Expr FieldName] -> [Expr Field]
toIndexedFields names = snd . mapAccumL go names where
   go names (GroupBy ns exprs) =
      (ns, GroupBy
           (map (getFieldIndex names) ns)
           (toIndexedFields (names \\ ns) exprs))
   go names Flatten =
      (names, Flatten)
   go names (Project xs) =
      (ns, Project $ map (fmap (getFieldIndex names)) xs)
      where
         ns = map getF xs
         getF (ProjField f)   = f
         getF (ProjValue _ f) = f
   go names (Aggregate xs) =
      (ns, Aggregate $ map (fmap (getFieldIndex names)) xs)
      where
         ns = [f | (_,f) <- xs]
