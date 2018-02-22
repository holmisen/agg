module Main where

import Common
import Config
import DataSet.InputSimple (readDataSet)
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

   ofs <- fromMaybe "\t" <$> lookupEnv "OFS"
   ifs <- fromMaybe "" <$> lookupEnv "IFS"

   let config = defaultConfig
          { configOutputSep = pack ofs
          , configInputSep = pack ifs }

   appRunWithConfig config $ do

--   print $ prepareExpressions program  -- DEBUG

      dataSet <- readDataSet =<< liftIO (Lazy.getContents)

      printDataSet $ runProgram program dataSet
