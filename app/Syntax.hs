module Syntax where

type CompUnit = [Stmt]

data Stmt
  = EmptyStmt
  | ExprStmt Expr
  | BlockStmt [Stmt]
  | BreakStmt
  | ContinueStmt
  deriving (Eq, Show)

data Expr
  = ConstInt Integer
  | ConstFloat Double
  | BinaryExpr BinaryOp Expr Expr
  | Var String
  | Call String [Expr]
  deriving (Eq, Show)

data BinaryOp = Plus | Minus | Times | Divide | Modulo
  deriving (Eq, Show)
