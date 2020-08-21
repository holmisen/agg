module Data where

import Common

import Data.Text (Text, empty, pack)
import qualified Data.Text.Read as T

--------------------------------------------------------------------------------

data Data
   = DataTxt !Text
   | DataDbl !Double
   deriving (Eq, Ord, Show)


nullData :: Data
nullData = DataTxt empty


textGetDouble :: Text -> Maybe Double
textGetDouble = either (const Nothing) (Just . fst) . T.signed T.double


dataGetDouble :: Data -> Maybe Double
dataGetDouble = get where
   get (DataDbl x) = Just x
   get (DataTxt x) = textGetDouble x


dataToText :: Data -> Text
dataToText (DataTxt x) = x
dataToText (DataDbl x) = pack (showFloat x)


showFloat :: (RealFrac a, Show a) => a -> String
showFloat x = if floor x == ceiling x then show (floor x) else show x


applyNumOp :: (Double -> Double -> Double) -> Data -> Data -> Data
applyNumOp f x y = 
   maybe nullData DataDbl $ f <$> dataGetDouble x <*> dataGetDouble y
