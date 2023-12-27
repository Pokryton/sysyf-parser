module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Data.Functor

import Lexer
import Syntax

expr = buildExpressionParser table unary
  where
    table = [ [binary "*" Mul, binary "/" Div, binary "%" Mod]
            , [binary "+" Add, binary "-" Sub]
            ]
    binary name op = Infix (reservedOp name $> (BinaryExpr op)) AssocLeft

unary = do
  ops <- many $ choice
           [ reservedOp "+" $> Pos
           , reservedOp "-" $> Neg
           , reservedOp "!" $> Not
           ]
  p <- primary
  return $ foldr UnaryExpr p ops

primary = try call <|> var <|> number <|> parens expr

number = try (ConstInt <$> integer) <|> (ConstFloat <$> float)

var = Var <$> identifier <*> many (brackets expr)

call = Call <$> identifier <*> parens (commaSep expr)

condExpr = buildExpressionParser table expr
  where
    table = [ [rel "==" Eq, rel "!=" Ne, rel "<" Lt, rel "<=" Le, rel ">" Gt, rel ">=" Ge]
            , [logic "&&" LAnd]
            , [logic "||" LOr]
            ]
    rel name op = Infix (reservedOp name $> RelExpr op) AssocLeft
    logic name op = Infix (reservedOp name $> LogicExpr op) AssocLeft

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
  reservedOp "="
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
    Var name index <- var
    reservedOp "="
    init <- initVal
    return $ ConstDef elemType name index init

varDefs = do
  elemType <- varType
  commaSep $ do
    Var name index <- var
    init <- optionMaybe (reservedOp "=" >> initVal)
    return $ VarDef elemType name index init

defs = (try constDefs <|> varDefs) <* reservedOp ";"

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
