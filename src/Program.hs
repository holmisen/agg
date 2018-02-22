module Program where

import DataSet
import Expr
import ExprNamedFields
import Interpreter
import ProgramTypes
import Record

--------------------------------------------------------------------------------

runProgram :: Program -> DataSet Record -> DataSet Record
runProgram p@(Program cs exprs) ds =
   let
      exprs' = snd (prepareExpressions p)
   in
      Interpreter.run exprs' ds


prepareExpressions :: Program -> (Env, [Expr Field])
prepareExpressions (Program cs exprs) =
   let
      env = envFromList (map fst cs)
   in
      resolveNames env exprs
