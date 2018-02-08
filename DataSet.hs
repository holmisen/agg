module DataSet
  ( DataSet(..)
  , isEmptyDataSet
  , printDataSet
  , readDataSet
  , readDataSetFromFile
  , module Data
  )
where

import Common
import Data
import Record

import Data.List (intercalate)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

--------------------------------------------------------------------------------

data DataSet a
   = Seq [a]
   | Group (Map Record (DataSet a))
   deriving (Show)

instance Functor DataSet where
   fmap f (Seq rs) = Seq (fmap f rs)
   fmap f (Group m) = Group (fmap (fmap f) m)


isEmptyDataSet :: DataSet a -> Bool
isEmptyDataSet = o where
   o (Seq xs) = null xs
   o (Group m) = null m

--------------------------------------------------------------------------------

printDataSet :: DataSet Record -> IO ()
printDataSet = printDataSet' 0


printDataSet' i (Seq rs) = do
   mapM_ (printRecord i) rs
   T.putStrLn T.empty
printDataSet' i (Group m) =
   printGroups i m


printRecord :: Int -> Record -> IO ()
printRecord i =
   output i . T.intercalate (T.pack "\t") . map dataToText . recordToList


printGroups :: Int -> Map Record (DataSet Record) -> IO ()
printGroups i m =
   mapM_ (uncurry $ printGroup i) (Map.toList m)


printGroup :: Int -> Record -> DataSet Record -> IO ()
printGroup i r ds = do
   printRecord i r
   printDataSet' (i+1) ds


output :: Int -> T.Text -> IO ()
output i s = T.putStrLn $ (T.replicate i indentation <> s)


indentation = T.pack "   "

--------------------------------------------------------------------------------

readDataSet :: L.Text -> DataSet Record
readDataSet =
   Seq . map (recordFromList . map (DataTxt . L.toStrict) . L.words) . L.lines


readDataSetFromFile :: FilePath -> IO (DataSet Record)
readDataSetFromFile fp = readDataSet <$> L.readFile fp
