module Parser where

import Text.Parsec
import Text.Parsec.Expr
import Data.Functor

import Lexer
import Syntax

expr = buildExpressionParser table unary
  where
    table = [ [binary "*" Times, binary "/" Divide, binary "%" Modulo]
            , [binary "+" Plus, binary "-" Minus]]
    binary name f = Infix (reservedOp name $> (BinaryExpr f)) AssocLeft

unary = (prefix "+" Positive)
     <|> (prefix "-" Negative)
     <|> (prefix "!" Not)
     <|> primary
  where
    prefix name f = reservedOp name >> UnaryExpr f <$> unary

primary = number <|> parens expr <|> try call <|> var

number = try (ConstInt <$> integer) <|> (ConstFloat <$> float)

var = Var <$> identifier

call = Call <$> identifier <*> parens (commaSep expr)

condExpr = buildExpressionParser table (JustExpr <$> expr)
  where
    table = [ [rel "==" Eq, rel "!=" Neq, rel "<" Lt, rel "<=" Le, rel ">" Gt, rel ">=" Ge]
            , [logic "&&" LAnd]
            , [logic "||" LOr]]
    rel name f = binary name (RelExpr f)
    logic name f = binary name (LogicExpr f)
    binary name f = Infix (reservedOp name $> f) AssocLeft

stmt = emptyStmt
    <|> exprStmt
    <|> ifStmt
    <|> whileStmt
    <|> breakStmt
    <|> continueStmt
    <|> blockStmt

emptyStmt = semi $> EmptyStmt

exprStmt = ExprStmt <$> expr <* semi

blockStmt = BlockStmt <$> braces (many stmt)

breakStmt = reserved "break" >> semi $> BreakStmt

continueStmt = reserved "continue" >> semi $> ContinueStmt

ifStmt = do
  reserved "if"
  cond <- parens condExpr
  true <- stmt
  false <- (reserved "else" >> stmt) <|> return EmptyStmt
  return $ IfStmt cond true false

whileStmt = do
  reserved "while"
  cond <- parens condExpr
  body <- stmt
  return $ WhileStmt cond body

compUnit = (many stmt) <* eof

parseCompUnit = parse compUnit
