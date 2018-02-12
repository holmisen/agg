module Program where

import DataSet
import Expr
import ExprParser
import Interpreter
import ProgramTypes
import Record

--------------------------------------------------------------------------------

runProgram :: Program -> DataSet Record -> DataSet Record
runProgram p@(Program cs exprs) ds =
   let
      exprs' = prepareExpressions p
   in
      Interpreter.run exprs' ds


prepareExpressions :: Program -> [Expr Field]
prepareExpressions (Program cs exprs) =
   let
      env = Expr.envFromList (map fst cs)
   in
      Expr.toIndexedFields env exprs
