module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Data.Functor

import Lexer
import Syntax

expr = addExpr

isOp s op = symbol s $> op

addOp = ("+" `isOp` Add) <|> ("-" `isOp` Sub)

addExpr = mulExpr `chainl1` (BinaryExpr <$> addOp)

mulOp = ("*" `isOp` Mul) <|> ("/" `isOp` Div) <|> ("%" `isOp` Mod)

mulExpr = unaryExpr `chainl1` (BinaryExpr <$> mulOp)

unaryOp = ("+" `isOp` Pos) <|> ("-" `isOp` Neg) <|> ("!" `isOp` Not)

unaryExpr = do
  ops <- many unaryOp
  p <- primaryExpr
  return $ foldr UnaryExpr p ops

primaryExpr = choice [try call, var, number, parens expr]

number = try (ConstInt <$> integer) <|> (ConstFloat <$> float)

var = Var <$> identifier <*> many (brackets expr)

call = Call <$> identifier <*> parens (commaSep expr)

relOp = choice
      [ "==" `isOp` Eq
      , "!=" `isOp` Ne
      , "<" `isOp` Lt
      , "<=" `isOp` Le
      , ">" `isOp` Gt
      , ">=" `isOp` Ge
      ]

relExpr = expr `chainl1` (RelExpr <$> relOp)

landExpr = relExpr `chainl1` (LogicExpr <$> ("&&" `isOp` LAnd))

lorExpr = landExpr `chainl1` (LogicExpr <$> ("||" `isOp` LOr))

condExpr = lorExpr

stmt = choice
      [ try exprStmt
      , assignStmt
      , emptyStmt
      , ifStmt
      , whileStmt
      , returnStmt
      , breakStmt
      , continueStmt
      , blockStmt
      ]

exprStmt = ExprStmt <$> expr <* semi

assignStmt = do
  Var name index <- var
  symbol "="
  value <- expr
  semi
  return $ AssignStmt name index value

emptyStmt = semi $> EmptyStmt

blockStmt = BlockStmt <$> block

blockItems = (pure <$> try stmt) <|> (map VarDefStmt <$> defs)

block = concat <$> braces (many blockItems)

breakStmt = reserved "break" >> semi $> BreakStmt

continueStmt = reserved "continue" >> semi $> ContinueStmt

returnStmt = ReturnStmt <$> (reserved "return" *> optionMaybe expr <* semi)

ifStmt = do
  reserved "if"
  cond <- parens condExpr
  true <- stmt
  false <- optionMaybe (reserved "else" >> stmt)
  return $ IfStmt cond true false

whileStmt = do
  reserved "while"
  cond <- parens condExpr
  body <- stmt
  return $ WhileStmt cond body

varType = (reserved "int" $> IntType) <|> (reserved "float" $> FloatType)

initVal = try (InitList <$> braces (commaSep initVal)) <|> (InitExpr <$> expr)

constDefs = do
  reserved "const"
  elemType <- varType
  commaSep $ do
    Var name index <- var
    symbol "="
    init <- initVal
    return $ ConstDef elemType name index init

varDefs = do
  elemType <- varType
  commaSep $ do
    Var name index <- var
    init <- optionMaybe (symbol "=" >> initVal)
    return $ VarDef elemType name index init

defs = (try constDefs <|> varDefs) <* semi

params = do
  elemType <- varType
  name <- identifier
  index <- optionMaybe (brackets (optional expr) >> many (brackets expr))
  return $ Param elemType name index

funcDef = do
  retType <- (Just <$> varType) <|> (reserved "void" $> Nothing)
  name <- identifier
  params <- parens (commaSep params)
  body <- block
  return $ FuncDef retType name params body

globalDefs = try (map Global <$> defs) <|> (pure <$> funcDef)

compUnit = concat <$> (many globalDefs) <* eof

parseFile = parseFromFile compUnit
