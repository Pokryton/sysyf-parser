module Parser where

import Text.Parsec
import Text.Parsec.Expr

import Lexer
import Syntax

binary s f = Infix (reservedOp s >> return (BinaryExpr f)) AssocLeft

table = [ [binary "*" Times, binary "/" Divide, binary "%" Modulo]
        , [binary "+" Plus, binary "-" Minus]]

expr = buildExpressionParser table primary

primary = number <|> parens expr <|> try call <|> var

number = try (ConstInt <$> integer) <|> (ConstFloat <$> float)

var = Var <$> identifier

call = Call <$> identifier <*> parens (commaSep expr)

stmt = try emptyStmt
    <|> try exprStmt
    <|> try breakStmt
    <|> try continueStmt
    <|> blockStmt

emptyStmt = semi >> return EmptyStmt

exprStmt = ExprStmt <$> expr <* semi

blockStmt = BlockStmt <$> braces (many stmt)

breakStmt = reserved "break" >> semi >> return BreakStmt

continueStmt = reserved "continue" >> semi >> return ContinueStmt

compUnit = spaces *> (many stmt) <* eof

parseCompUnit = parse compUnit
