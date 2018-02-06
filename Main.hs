module Main where

import DataSet (printDataSet, readDataSet)
import ExprParser (parseProgramFile)
import Program (runProgram, prepareExpressions)

import System.Environment (getArgs)
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

--   print $ prepareExpressions program  -- DEBUG

   dataSet <- readDataSet <$> Lazy.getContents

   printDataSet $ runProgram program dataSet
