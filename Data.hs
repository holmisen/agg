module Data where

import Data.Text (Text, empty, pack)
import qualified Data.Text.Read as T

--------------------------------------------------------------------------------

data Data
   = DataTxt !Text
   | DataDbl !Double
   deriving (Eq, Ord, Show)


nullData :: Data
nullData = DataTxt empty


textReadDouble :: Double -> Text -> Double
textReadDouble def x = case T.signed T.double x of
   Left  _     -> def
   Right (n,_) -> n


dataToDouble :: Double -> Data -> Double
dataToDouble def = get where
   get (DataDbl x) = x
   get (DataTxt x) = textReadDouble def x


dataToText :: Data -> Text
dataToText (DataTxt x) = x
dataToText (DataDbl x) = pack (show x)
