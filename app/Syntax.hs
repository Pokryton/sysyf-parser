module Syntax where

type CompUnit = [Stmt]

data Stmt
  = EmptyStmt
  | ExprStmt Expr
  | BlockStmt [Stmt]
  | IfStmt CondExpr Stmt Stmt
  | WhileStmt CondExpr Stmt
  | BreakStmt
  | ContinueStmt
  deriving (Eq, Show)

data Expr
  = ConstInt Integer
  | ConstFloat Double
  | BinaryExpr BinaryOp Expr Expr
  | UnaryExpr UnaryOp Expr
  | Var String
  | Call String [Expr]
  deriving (Eq, Show)

data BinaryOp = Plus | Minus | Times | Divide | Modulo
  deriving (Eq, Show)

data UnaryOp = Positive | Negative | Not
  deriving (Eq, Show)

data CondExpr
  = JustExpr Expr
  | RelExpr RelOp CondExpr CondExpr
  | LogicExpr LogicOp CondExpr CondExpr
  deriving (Eq, Show)

data RelOp = Eq | Neq | Lt | Le | Gt | Ge
  deriving (Eq, Show)

data LogicOp = LAnd | LOr
  deriving (Eq, Show)
