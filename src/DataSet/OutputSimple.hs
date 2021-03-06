module DataSet.OutputSimple
  ( printDataSet )
where

import Common
import Config
import DataSet
import Record

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

--------------------------------------------------------------------------------

printDataSet :: DataSet Record -> AppT IO ()
printDataSet = printDataSet' 0


printDataSet' i (Seq rs) = do
   mapM_ (printRecord i) rs
   liftIO $ T.putStrLn T.empty
printDataSet' i (Group m) =
   printGroups i m


printRecord :: Int -> Record -> AppT IO ()
printRecord i r = do
   sep <- asks configOutputSep
   output i . T.intercalate sep . map dataToText . recordToList $ r


printGroups :: Int -> Map Record (DataSet Record) -> AppT IO ()
printGroups i m =
   mapM_ (uncurry $ printGroup i) (Map.toList m)


printGroup :: Int -> Record -> DataSet Record -> AppT IO ()
printGroup i r ds = do
   printRecord i r
   printDataSet' (i+1) ds


output :: Int -> Text -> AppT IO ()
output i s = do
   ind <- asks configOutputIndent
   liftIO $ T.putStrLn (T.replicate i ind <> s)
