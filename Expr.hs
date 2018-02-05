module Expr where

import Types

--------------------------------------------------------------------------------

type Field = Int

data AggFun = AggSum | AggProd | AggCount
   deriving (Eq, Ord, Show)

data ProjExpr = ProjField Field | ProjValue Data
   deriving (Eq, Show)

data Expr
   = GroupBy [Field] [Expr]
   | Flatten
   | Project [ProjExpr]
   | Aggregate [(Field, AggFun)]
   deriving (Eq, Show)
