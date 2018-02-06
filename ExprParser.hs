module ExprParser
  ( parseExprs
  , parseExprsFile
  )
where

import Expr

import System.Exit (die)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

--------------------------------------------------------------------------------

type Parser a = Parsec String () a

type ExprParser = Parser (Expr Field)


lexer = T.makeTokenParser emptyDef
   { T.reservedNames = ["by", "group", "flatten", "aggregate", "project"]
   , T.commentLine = "#"
   }

parens      = T.parens lexer
braces      = T.braces lexer
identifier  = T.identifier lexer
reserved    = T.reserved lexer
symbol      = T.symbol lexer
natural     = T.natural lexer
comma       = T.comma lexer
semi        = T.semi lexer

--------------------------------------------------------------------------------

pField :: Parser Field
pField = fromIntegral <$> natural <?> "field"


pAggFun :: Parser AggFun
pAggFun = choice
   [ const AggSum <$> symbol "sum"
   , const AggProd <$> symbol "product"
   , const AggCount <$> symbol "count"
   ]


pGroupBy :: ExprParser
pGroupBy = do
   reserved "group"
   reserved "by"
   fs <- pField `sepBy1` comma
   es <- braces pExprs
   return $ GroupBy fs es


pFlatten :: ExprParser
pFlatten = const Flatten <$> reserved "flatten"


pAggregate :: ExprParser
pAggregate = Aggregate <$>
   (reserved "aggregate" *> pAggField `sepBy1` comma)


pAggField :: Parser (AggFun, Field)
pAggField = do
   op <- pAggFun
   f <- parens pField
   return (op,f)


pProject :: ExprParser
pProject = do
   reserved "project"
   fs <- pProjExpr `sepBy1` comma
   return $ Project fs


pProjExpr :: Parser (ProjExpr Field)
pProjExpr = (ProjField <$> pField) -- TODO: Parse values


pExpr :: ExprParser
pExpr = choice [pAggregate, pGroupBy, pFlatten, pProject]


pExprs :: Parser [Expr Field]
pExprs = many pExpr -- `sepBy` semi

--------------------------------------------------------------------------------

parseExprs :: SourceName -> String -> Either ParseError [Expr Field]
parseExprs = parse (pExprs <* eof)


parseExprsFile :: FilePath -> IO [Expr Field]
parseExprsFile filePath = do
   result <- parseExprs filePath <$> readFile filePath
   case result of
      Left err ->
         die $ show err
      Right expr ->
         return expr
