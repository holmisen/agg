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


textGetDouble :: Text -> Maybe Double
textGetDouble = either (const Nothing) (Just . fst) . T.signed T.double


dataGetDouble :: Data -> Maybe Double
dataGetDouble = get where
   get (DataDbl x) = Just x
   get (DataTxt x) = textGetDouble x


dataToText :: Data -> Text
dataToText (DataTxt x) = x
dataToText (DataDbl x) = pack (show x)
