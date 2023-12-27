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

var = Var <$> lval

call = Call <$> identifier <*> parens (commaSep expr)

lval = LVal <$> identifier <*> many (brackets expr)

condExpr = buildExpressionParser table (JustExpr <$> expr)
  where
    table = [ [rel "==" Eq, rel "!=" Neq, rel "<" Lt, rel "<=" Le, rel ">" Gt, rel ">=" Ge]
            , [logic "&&" LAnd]
            , [logic "||" LOr]]
    rel name f = binary name (RelExpr f)
    logic name f = binary name (LogicExpr f)
    binary name f = Infix (reservedOp name $> f) AssocLeft

stmt = try exprStmt
    <|> assignStmt
    <|> emptyStmt
    <|> ifStmt
    <|> whileStmt
    <|> returnStmt
    <|> breakStmt
    <|> continueStmt
    <|> blockStmt

emptyStmt = semi $> EmptyStmt

exprStmt = ExprStmt <$> expr <* semi

blockStmt = BlockStmt <$> block

block = concat <$> braces (many ((pure . Stmt <$> try stmt) <|> (map Decl <$> decls)))

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

constDecls = do
  reserved "const"
  elemType <- varType
  commaSep $ do
    LVal name index <- lval
    reservedOp "="
    init <- initVal
    return $ ConstDecl elemType name index init

varDecls = do
  elemType <- varType
  commaSep $ do
    LVal name index <- lval
    init <- optionMaybe (reservedOp "=" >> initVal)
    return $ VarDecl elemType name index init

decls = try constDecls <|> varDecls

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

compUnit = concat <$> (many ((pure . Func <$> try funcDef) <|> (map GVar <$> decls))) <* eof

parseCompUnit = parse compUnit
