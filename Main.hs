module Main where

import DataSet (printDataSet, readDataSet)
import ExprParser (parseExprsFile)
import Interpreter (run)

import Data.Text.Lazy.IO as Lazy
import System.Environment (getArgs)
import System.Exit (die)

--------------------------------------------------------------------------------

main = do
   exprFile <- do
      args <- getArgs
      case args of
         [arg1] ->
            return arg1
         _ ->
            die $ "USAGE <exprFile>"

   expr <- parseExprsFile exprFile

   dataSet <- readDataSet <$> Lazy.getContents

   printDataSet $ run expr dataSet
