module Expr where

import Data
import Types

--------------------------------------------------------------------------------

type Field = Int

newtype FieldName = FieldName String
   deriving (Eq, Ord, Show)

data AggFun = AggSum | AggProd | AggCount
   deriving (Eq, Ord, Show)

data ProjExpr f = ProjField f | ProjValue Data
   deriving (Eq, Show)

-- | f is the type of field identifiers
data Expr f
   = GroupBy [f] [Expr f]
   | Flatten
   | Project [ProjExpr f]
   | Aggregate [(f, AggFun)]
   deriving (Eq, Show)
