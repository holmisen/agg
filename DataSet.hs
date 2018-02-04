module DataSet where

import Types

import Data.List (intercalate)
import qualified Data.Map as Map

--------------------------------------------------------------------------------

type Record = [Data]

data DataSet a
   = Seq [a]
   | Group (Map Record (DataSet a))
   deriving (Show)

instance Functor DataSet where
   fmap f (Seq rs) = Seq (fmap f rs)
   fmap f (Group m) = Group (fmap (fmap f) m)

--------------------------------------------------------------------------------

printDataSet :: DataSet Record -> IO ()
printDataSet = printDataSet' 0


printDataSet' i (Seq rs) = do
   mapM_ (printRecord i) rs
   putStrLn ""
printDataSet' i (Group m) =
   printGroups i m


printRecord :: Int -> Record -> IO ()
printRecord i = output i . intercalate "\t"


printGroups :: Int -> Map Record (DataSet Record) -> IO ()
printGroups i m =
   mapM_ (uncurry $ printGroup i) (Map.toList m)


printGroup :: Int -> Record -> DataSet Record -> IO ()
printGroup i r ds = do
   printRecord i r
   printDataSet' (i+1) ds


output :: Int -> String -> IO ()
output i s = putStrLn $ concat (replicate i "   ") ++ s

--------------------------------------------------------------------------------

readDataSet :: String -> DataSet Record
readDataSet = Seq . map words . lines


readDataSetFromFile :: FilePath -> IO (DataSet Record)
readDataSetFromFile fp = readDataSet <$> readFile fp
