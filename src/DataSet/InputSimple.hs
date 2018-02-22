-- | Lazy reading data sets.
--
module DataSet.InputSimple
  ( readDataSet )
where

import Config
import DataSet hiding (readDataSet)
import Record

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

--------------------------------------------------------------------------------

readDataSet :: Monad m => L.Text -> AppT m (DataSet Record)
readDataSet input = do
   sep <- asks configInputSep
   return $ readDataSetWith sep input


readDataSetWith :: T.Text -> L.Text -> DataSet Record
readDataSetWith sep = Seq . map (readRecord sep) . L.lines


readRecord :: T.Text -> L.Text -> Record
readRecord sep = recordFromList . map (DataTxt . L.toStrict) . fields
   where
      fields
         | T.null sep = L.words
         | otherwise  = L.splitOn (L.fromStrict sep)
