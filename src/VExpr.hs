{-# LANGUAGE DeriveFunctor #-}

-- | Value expressions.
--
module VExpr where

import Common
import Data

--------------------------------------------------------------------------------

data VExpr f
   = VField f
   | VData Data
   | VAdd (VExpr f) (VExpr f)
   | VSub (VExpr f) (VExpr f)
   | VMul (VExpr f) (VExpr f)
   | VDiv (VExpr f) (VExpr f)
   | VDec Int Int (VExpr f)
   deriving (Eq, Functor, Show)


-- getVExprFields :: VExpr f -> [f]
-- getVExprFields (VField f) = [f]
-- getVExprFields (VData _) = []
-- getVExprFields (VAdd x y) = getVExprFields x <> getVExprFields y
-- getVExprFields (VSub x y) = getVExprFields x <> getVExprFields y
-- getVExprFields (VMul x y) = getVExprFields x <> getVExprFields y
-- getVExprFields (VDiv x y) = getVExprFields x <> getVExprFields y

