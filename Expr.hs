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

data Order = Asc | Desc
   deriving (Eq, Ord, Show)

-- | f is the type of field identifiers
data Expr f
   = GroupBy [f] [Expr f]
   | Flatten
   | Project [ProjExpr f]
   | Aggregate [(AggFun, f)]
   | SortBy [(Order, f)]
   deriving (Eq, Show)

--------------------------------------------------------------------------------

data Env = Env [FieldName] (Maybe Env) deriving Show

envFromList :: [FieldName] -> Env
envFromList ns = Env ns Nothing

flattenEnv :: Env -> [FieldName]
flattenEnv (Env ns e) = ns ++ maybe [] flattenEnv e

--------------------------------------------------------------------------------

getFieldIndex :: [FieldName] -> FieldName -> Int
getFieldIndex ns n@(FieldName x) =
   fromMaybe (error $ "No such name `" ++ x ++ "`") (List.elemIndex n ns)


-- | Translate field names into indices.
--
toIndexedFields :: Env -> [Expr FieldName] -> [Expr Field]
toIndexedFields env = snd . compute env


compute env = mapAccumL go env where
   go (Env names _) (GroupBy ns exprs) =
      ( Env ns (Just subEnv')
      , GroupBy (map (getFieldIndex names) ns) exprs')
      where
         subEnv = Env (names \\ ns) Nothing
         (subEnv',exprs') = compute subEnv exprs
   go env Flatten =
      ( Env (flattenEnv env) Nothing
      , Flatten )
   go (Env names env) (Project xs) =
      ( Env ns env
      , Project $ map (fmap (getFieldIndex names)) xs )
      where
         ns = map getF xs
         getF (ProjField f)   = f
         getF (ProjValue _ f) = f
   go (Env names env) (Aggregate xs) =
      ( Env ns env
      , Aggregate $ map (fmap (getFieldIndex names)) xs)
      where
         ns = [f | (_,f) <- xs]
   go env@(Env names _) (SortBy fs) =
      ( env
      , SortBy $ map (fmap (getFieldIndex names)) fs )
