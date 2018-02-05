module Types
   ( Map
   , Data
   , nullData
   , readNum
   , numToData
   , module Export
   )
where

import Data.Monoid as Export
import Data.Map (Map)
import Data.Text (Text, empty, pack)
import qualified Data.Text.Read as T

--------------------------------------------------------------------------------

type Data = Text

nullData = empty


readNum :: Double -> Data -> Double
readNum def x = case T.signed T.double x of
   Left  _     -> def
   Right (n,_) -> n


numToData :: (Num a, Show a) => a -> Data
numToData = pack . show
