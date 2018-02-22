{-# LANGUAGE DeriveFunctor #-}

module Expr where

import VExpr

--------------------------------------------------------------------------------

type Field = Int

newtype FieldName = FieldName String
   deriving (Eq, Ord, Show)

data AggFun = AggSum | AggProd | AggCount | AggMax | AggMin
   deriving (Eq, Ord, Show)

data ProjExpr f = ProjExpr (VExpr f) (Maybe f)
   deriving (Eq, Functor, Show)

data Order = Asc | Desc
   deriving (Eq, Ord, Show)

-- | f is the type of field identifiers
data Expr f
   = GroupBy [f] [Expr f]
   | Flatten
   | Project [ProjExpr f]
   | Aggregate [(AggFun, f)]
   | SortBy [(Order, f)]
   | TakeN Int
   | CrossJoin [Expr f]
   deriving (Eq, Show)

