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

lexer = T.makeTokenParser emptyDef
   { T.reservedNames = ["by", "group", "flatten", "aggregate"]
   , T.commentLine = "//"
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
pField = fromIntegral <$> natural


pAggFun :: Parser AggFun
pAggFun = (const AggSum <$> symbol "sum")
      <|> (const AggProd <$> symbol "product")


pGroupBy :: Parser Expr
pGroupBy = do
   reserved "group"
   reserved "by"
   fs <- pField `sepBy1` comma
   es <- braces pExprs
   return $ GroupBy fs es


pFlatten :: Parser Expr
pFlatten = const Flatten <$> reserved "flatten"


pAggregate :: Parser Expr
pAggregate = Aggregate <$>
   (reserved "aggregate" *> pAggField `sepBy1` comma)


pAggField = do
   f <- pField
   reserved "by"
   op <- pAggFun
   return (f,op)


pExpr :: Parser Expr
pExpr = choice [pAggregate, pGroupBy, pFlatten]


pExprs :: Parser [Expr]
pExprs = many pExpr -- `sepBy` semi

--------------------------------------------------------------------------------

parseExprs :: SourceName -> String -> Either ParseError [Expr]
parseExprs = parse (pExprs <* eof)


parseExprsFile :: FilePath -> IO [Expr]
parseExprsFile filePath = do
   result <- parseExprs filePath <$> readFile filePath
   case result of
      Left err ->
         die $ show err
      Right expr ->
         return expr
