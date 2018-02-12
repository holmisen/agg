module Main where

import Common
import Config
import DataSet (readDataSet)
import DataSet.OutputSimple (printDataSet)
import ExprParser (parseProgramFile)
import Program (runProgram, prepareExpressions)

import Data.Text (pack)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)

import qualified Data.Text.Lazy.IO as Lazy

--------------------------------------------------------------------------------

main = do
   programFile <- do
      args <- getArgs
      case args of
         [arg1] ->
            return arg1
         _ ->
            die $ "USAGE <exprFile>"

   program <- parseProgramFile programFile

   dataSet <- readDataSet <$> Lazy.getContents

   ofs <- fromMaybe "\t" <$> lookupEnv "OFS"

   let config = defaultConfig { configOutputSep = pack ofs }

   appRunWithConfig config $ do

--   print $ prepareExpressions program  -- DEBUG

      printDataSet $ runProgram program dataSet
