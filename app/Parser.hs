module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

import Lexer
import Syntax

expr = addExpr

symb op s = op <$ symbol s
keyword f s = f <$ reserved s

addOp = (Add `symb` "+") <|> (Sub `symb` "-")

addExpr = mulExpr `chainl1` (BinaryExpr <$> addOp)

mulOp = (Mul `symb` "*") <|> (Div `symb` "/") <|> (Mod `symb` "%")

mulExpr = unaryExpr `chainl1` (BinaryExpr <$> mulOp)

unaryOp = (Pos `symb` "+") <|> (Neg `symb` "-") <|> (Not `symb` "!")

unaryExpr = do
  ops <- many unaryOp
  p <- primaryExpr
  return $ foldr UnaryExpr p ops

primaryExpr = choice [try call, var, number, parens expr]

number = try (ConstInt <$> integer) <|> (ConstFloat <$> float)

var = Var <$> identifier <*> many (brackets expr)

call = Call <$> identifier <*> parens (commaSep expr)

relOp = choice
      [ Eq `symb` "=="
      , Ne `symb` "!="
      , Lt `symb` "<"
      , Le `symb` "<="
      , Gt `symb` ">"
      , Ge `symb` ">="
      ]

relExpr = expr `chainl1` (RelExpr <$> relOp)

landExpr = relExpr `chainl1` (LogicExpr <$> (LAnd `symb` "&&"))

lorExpr = landExpr `chainl1` (LogicExpr <$> (LOr `symb` "||"))

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

emptyStmt = EmptyStmt <$ semi

blockStmt = BlockStmt <$> block

blockItems = try (pure <$> stmt) <|> (map VarDefStmt <$> defs)

block = concat <$> braces (many blockItems)

breakStmt = BreakStmt `keyword` "break" <* semi

continueStmt = ContinueStmt `keyword` "continue" <* semi

returnStmt = ReturnStmt `keyword` "return" <*> optionMaybe expr <* semi

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

varType = (IntType `keyword` "int") <|> (FloatType `keyword` "float")

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
  retType <- (Just <$> varType) <|> (Nothing `keyword` "void")
  name <- identifier
  params <- parens (commaSep params)
  body <- block
  return $ FuncDef retType name params body

globalDefs = try (map Global <$> defs) <|> (pure <$> funcDef)

compUnit = concat <$> (whiteSpace *> many globalDefs <* eof)

parseFile = parseFromFile compUnit
