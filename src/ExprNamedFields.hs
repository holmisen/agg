module ExprNamedFields
  ( Env(..)
  , envFromList
  , flattenEnv

  , toIndexedFields
  , resolveNames
  )
where

import Common
import Data
import Expr
import VExpr

import Data.List as List

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
toIndexedFields env = snd . resolveNames env


-- | Translate field names into indices. Also returns the environment
-- as it is after the last expression.
--
resolveNames :: Env -> [Expr FieldName] -> (Env, [Expr Int])
resolveNames env = mapAccumL go env where
   go (Env names _) (GroupBy ns exprs) =
      ( Env ns (Just subEnv')
      , GroupBy (map (getFieldIndex names) ns) exprs')
      where
         subEnv = Env (names \\ ns) Nothing
         (subEnv',exprs') = resolveNames subEnv exprs
   go env Flatten =
      ( Env (flattenEnv env) Nothing
      , Flatten )
   go (Env names env) (Project pexprs) =
      ( Env names' env
      , Project pexprs' )
      where
         pexprs' = [ ProjExpr x' Nothing
                   | ProjExpr x _ <- pexprs
                   , let x' = fmap (getFieldIndex names) x
                   ]
         names' = map get pexprs
         get (ProjExpr _          (Just n')) = n'
         get (ProjExpr (VField n) Nothing  ) = n
         get _ = error $ "Missing name for column"
   go (Env names env) (Aggregate xs) =
      ( Env ns env
      , Aggregate $ map (fmap (getFieldIndex names)) xs)
      where
         ns = [f | (_,f) <- xs]
   go env@(Env names _) (SortBy fs) =
      ( env
      , SortBy $ map (fmap (getFieldIndex names)) fs )
   go env (TakeN n) =
      ( env
      , TakeN n )
   go env@(Env ns _) (CrossJoin exprs) =
      ( Env (ns <> ns') Nothing
      , CrossJoin exprs' )
      where
         (env',exprs') = resolveNames env exprs
         ns' = flattenEnv env'
