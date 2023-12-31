module Syntax where

newtype CompUnit = CompUnit [GlobalDef]
  deriving (Eq, Show)

data GlobalDef
  = FuncDef (Maybe VarType) String [Param] Block
  | Global Def
  deriving (Eq, Show)

data Def
  = ConstDef VarType String [Expr] InitVal
  | VarDef VarType String [Expr] (Maybe InitVal)
  deriving (Eq, Show)

data InitVal = InitExpr Expr | InitList [InitVal]
  deriving (Eq, Show)

data Param = Param VarType String (Maybe [Expr])
  deriving (Eq, Show)

data VarType = IntType | FloatType
  deriving (Eq, Show)

type Block = [Stmt]

data Stmt
  = EmptyStmt
  | ExprStmt Expr
  | BlockStmt Block
  | IfStmt Expr Stmt (Maybe Stmt)
  | WhileStmt Expr Stmt
  | DefStmt Def
  | ReturnStmt (Maybe Expr)
  | AssignStmt String [Expr] Expr
  | BreakStmt
  | ContinueStmt
  deriving (Eq, Show)

data Expr
  = ConstInt Integer
  | ConstFloat Double
  | BinaryExpr BinaryOp Expr Expr
  | UnaryExpr UnaryOp Expr
  | Var String [Expr]
  | Call String [Expr]
  | RelExpr RelOp Expr Expr
  | LogicExpr LogicOp Expr Expr
  deriving (Eq, Show)

data BinaryOp = Add | Sub | Mul | Div | Mod
  deriving (Eq, Show)

data UnaryOp = Pos | Neg | Not
  deriving (Eq, Show)

data RelOp = Eq | Ne | Lt | Le | Gt | Ge
  deriving (Eq, Show)

data LogicOp = LAnd | LOr
  deriving (Eq, Show)
