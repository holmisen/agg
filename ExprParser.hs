module ExprParser
  ( parseExprs
  , parseProgram
  , parseProgramFile
  )
where

import Expr
import ProgramTypes

import System.Exit (die)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

--------------------------------------------------------------------------------

parseProgram :: SourceName -> String -> Either ParseError Program
parseProgram = parse (pProgram <* eof)


parseProgramFile :: FilePath -> IO Program
parseProgramFile filePath = do
   result <- parseProgram filePath <$> readFile filePath
   case result of
      Left err ->
         die $ show err
      Right expr ->
         return expr

--------------------------------------------------------------------------------

type Parser a = Parsec String () a

type ExprParser = Parser (Expr FieldName)


lexer = T.makeTokenParser emptyDef
   { T.reservedNames = ["by", "group", "flatten", "aggregate", "project", "columns", "sort", "take"]
   , T.commentLine = "#"
   , T.identStart = upper
   , T.identLetter = alphaNum
   }

parens      = T.parens lexer
braces      = T.braces lexer
brackets    = T.brackets lexer
identifier  = T.identifier lexer
reserved    = T.reserved lexer
symbol      = T.symbol lexer
natural     = T.natural lexer
comma       = T.comma lexer
semi        = T.semi lexer

--------------------------------------------------------------------------------

pField :: Parser FieldName
pField = (FieldName <$> identifier) <?> "field" where


pType :: Parser DataType
pType = choice
   [ const TypeText <$> symbol "text"
   , const TypeDouble <$> symbol "double" ]


pAggFun :: Parser AggFun
pAggFun = choice
   [ const AggSum <$> try (symbol "sum")
   , const AggProd <$> try (symbol "product")
   , const AggCount <$> try (symbol "count")
   , const AggMax <$> try (symbol "max")
   , const AggMin <$> try (symbol "min")
   ]


pGroupBy :: ExprParser
pGroupBy = do
   reserved "group"
   reserved "by"
   fs <- brackets (pField `sepBy1` comma)
   es <- braces pExprs
   return $ GroupBy fs es


pFlatten :: ExprParser
pFlatten = const Flatten <$> reserved "flatten"


pAggregate :: ExprParser
pAggregate = do
   reserved "aggregate"
   Aggregate <$> brackets (pAggField `sepBy1` comma)


pAggField :: Parser (AggFun, FieldName)
pAggField = do
   op <- pAggFun
   f <- parens pField
   return (op,f)


pProject :: ExprParser
pProject = do
   reserved "project"
   fs <- brackets (pProjExpr `sepBy1` comma)
   return $ Project fs


pProjExpr :: Parser (ProjExpr FieldName)
pProjExpr = (ProjField <$> pField) -- TODO: Parse values


pSortField :: Parser (Order, FieldName)
pSortField = do
   f <- pField
   o <- option Asc (const Desc <$> symbol "desc")
   return (o,f)


pSortBy :: ExprParser
pSortBy = do
   reserved "sort"
   reserved "by"
   SortBy <$> brackets (pSortField `sepBy1` comma)


pTakeN :: ExprParser
pTakeN = do
   reserved "take"
   TakeN . fromIntegral <$> natural


pExpr :: ExprParser
pExpr = choice [pAggregate, pGroupBy, pFlatten, pProject, pSortBy, pTakeN]


pExprs :: Parser [Expr FieldName]
pExprs = many pExpr -- `sepBy` semi


pColumnDef :: Parser (FieldName, DataType)
pColumnDef = do
   n <- pField
   t <- option TypeText (parens pType)
   return (n,t)


pColumnsDef :: Parser [(FieldName, DataType)]
pColumnsDef = do
   reserved "columns"
   brackets (pColumnDef `sepBy1` comma)


pProgram :: Parser Program
pProgram = do
   cs <- pColumnsDef
   xs <- pExprs
   return $ Program cs xs

--------------------------------------------------------------------------------

parseExprs :: SourceName -> String -> Either ParseError [Expr FieldName]
parseExprs = parse (pExprs <* eof)


-- parseExprsFile :: FilePath -> IO [Expr FieldName]
-- parseExprsFile filePath = do
--    result <- parseExprs filePath <$> readFile filePath
--    case result of
--       Left err ->
--          die $ show err
--       Right expr ->
--          return expr
