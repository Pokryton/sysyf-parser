module Parser where

import Text.Parsec
import Text.Parsec.Expr
import Data.Functor

import Lexer
import Syntax

expr = buildExpressionParser table unary
  where
    table = [ [binary "*" Times, binary "/" Divide, binary "%" Modulo]
            , [binary "+" Plus, binary "-" Minus]
            ]
    binary name f = Infix (reservedOp name $> (BinaryExpr f)) AssocLeft

unary = do
  ops <- many $ choice
           [ reservedOp "+" $> Positive
           , reservedOp "-" $> Negative
           , reservedOp "!" $> Not
           ]
  p <- primary
  return $ foldr UnaryExpr p ops

primary = try call <|> var <|> number <|> parens expr

number = try (ConstInt <$> integer) <|> (ConstFloat <$> float)

var = Var <$> lval

call = Call <$> identifier <*> parens (commaSep expr)

lval = LVal <$> identifier <*> many (brackets expr)

condExpr = buildExpressionParser table (JustExpr <$> expr)
  where
    table = [ [rel "==" Eq, rel "!=" Neq, rel "<" Lt, rel "<=" Le, rel ">" Gt, rel ">=" Ge]
            , [logic "&&" LAnd]
            , [logic "||" LOr]
            ]
    rel name f = Infix (reservedOp name $> RelExpr f) AssocLeft
    logic name f = Infix (reservedOp name $> LogicExpr f) AssocLeft

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

emptyStmt = semi $> EmptyStmt

exprStmt = ExprStmt <$> expr <* semi

blockStmt = BlockStmt <$> block

blockItems = (pure <$> try stmt) <|> (map VarDefStmt <$> defs)

block = concat <$> braces (many blockItems)

breakStmt = reserved "break" >> semi $> BreakStmt

continueStmt = reserved "continue" >> semi $> ContinueStmt

returnStmt = ReturnStmt <$> (reserved "return" *> optionMaybe expr <* semi)

assignStmt = Assign <$> lval <*> (reservedOp "=" *> expr <* semi)

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

varType = (reserved "int" $> IntType) <|> (reserved "float" $> FloatType)

initVal = try (InitList <$> braces (commaSep initVal)) <|> (InitExpr <$> expr)

constDefs = do
  reserved "const"
  elemType <- varType
  commaSep $ do
    LVal name index <- lval
    reservedOp "="
    init <- initVal
    return $ ConstDef elemType name index init

varDefs = do
  elemType <- varType
  commaSep $ do
    LVal name index <- lval
    init <- optionMaybe (reservedOp "=" >> initVal)
    return $ VarDef elemType name index init

defs = (try constDefs <|> varDefs) <* reservedOp ";"

params = do
  elemType <- varType
  name <- identifier
  index <- optionMaybe ((brackets (optional expr)) >> many (brackets expr))
  return $ Param elemType name index

funcDef = do
  retType <- (Just <$> varType) <|> (reserved "void" $> Nothing)
  name <- identifier
  params <- parens (commaSep params)
  body <- block
  return $ FuncDef retType name params body

globalDefs = (pure <$> try funcDef) <|> defs

compUnit = concat <$> (many globalDefs) <* eof

parseCompUnit = parse compUnit
