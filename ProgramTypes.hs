module ProgramTypes where

import Expr

--------------------------------------------------------------------------------

data Program = Program
   { columnDefs :: [(FieldName, DataType)]
   , expressions :: [Expr FieldName]
   }
   deriving Show


data DataType
   = TypeText
   | TypeDouble
   deriving (Eq, Ord, Show)
