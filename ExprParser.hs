module ExprParser
  ( parseExprs
  , parseProgram
  , parseProgramFile
  )
where

import Data
import Expr
import VExpr
import ProgramTypes

import System.Exit (die)
import Text.Parsec
import Text.Parsec.Expr
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
   { T.reservedNames = ["by", "group", "flatten", "aggregate", "project", "columns", "sort", "take", "cross", "join"]
   , T.commentLine = "#"
   , T.identStart = upper
   , T.identLetter = alphaNum
   , T.reservedOpNames = ["+","-","*","/"]
   }

parens         = T.parens lexer
braces         = T.braces lexer
brackets       = T.brackets lexer
identifier     = T.identifier lexer
reserved       = T.reserved lexer
symbol         = T.symbol lexer
natural        = T.natural lexer
comma          = T.comma lexer
semi           = T.semi lexer
float          = T.float lexer
naturalOrFloat = T.naturalOrFloat lexer
reservedOp     = T.reservedOp lexer

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
pProjExpr = do
   x <- pVExpr
   n <- case x of
           VField {} ->
              optionMaybe (reserved "as" *> pField)
           _ ->
              Just <$> (reserved "as" *> pField)
   return $ ProjExpr x n


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


pCrossJoin :: ExprParser
pCrossJoin = do
   reserved "cross"
   reserved "join"
   CrossJoin <$> braces pExprs


pExpr :: ExprParser
pExpr = choice [pAggregate, pGroupBy, pFlatten, pProject, pSortBy, pTakeN, pCrossJoin]


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

--------------------------------------------------------------------------------
-- Expression parser

pData :: Parser Data
pData = choice [DataDbl . either fromIntegral id <$> naturalOrFloat] -- TODO: Text


pVExpr :: Parser (VExpr FieldName)
pVExpr = buildExpressionParser optable term


term = choice [parens pVExpr, VField <$> pField, VData <$> pData]

optable = [ [binary "*" VMul AssocLeft, binary "/" VDiv AssocLeft ]
          , [binary "+" VAdd AssocLeft, binary "-" VSub AssocLeft ]
          ]

binary name fun assoc = Infix (reservedOp name >> return fun) assoc
