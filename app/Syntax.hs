module Syntax where

type CompUnit = [Def]

data Def
  = ConstDef VarType String [Expr] InitVal
  | VarDef VarType String [Expr] (Maybe InitVal)
  | FuncDef RetType String [Param] Block
  deriving (Eq, Show)

data InitVal = InitExpr Expr | InitList [InitVal]
  deriving (Eq, Show)

data Param = Param VarType String (Maybe [Expr])
  deriving (Eq, Show)

data VarType = IntType | FloatType
  deriving (Eq, Show)

type RetType = Maybe VarType

data Stmt
  = EmptyStmt
  | ExprStmt Expr
  | BlockStmt Block
  | IfStmt CondExpr Stmt Stmt
  | WhileStmt CondExpr Stmt
  | VarDefStmt Def
  | ReturnStmt (Maybe Expr)
  | Assign LVal Expr
  | BreakStmt
  | ContinueStmt
  deriving (Eq, Show)

type Block = [Stmt]

data Expr
  = ConstInt Integer
  | ConstFloat Double
  | BinaryExpr BinaryOp Expr Expr
  | UnaryExpr UnaryOp Expr
  | Var LVal
  | Call String [Expr]
  deriving (Eq, Show)

data LVal = LVal String [Expr]
  deriving (Eq, Show)

data BinaryOp = Add | Sub | Mul | Div | Mod
  deriving (Eq, Show)

data UnaryOp = Pos | Neg | Not
  deriving (Eq, Show)

data CondExpr
  = JustExpr Expr
  | RelExpr RelOp CondExpr CondExpr
  | LogicExpr LogicOp CondExpr CondExpr
  deriving (Eq, Show)

data RelOp = Eq | Ne | Lt | Le | Gt | Ge
  deriving (Eq, Show)

data LogicOp = LAnd | LOr
  deriving (Eq, Show)
