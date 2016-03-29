module Ast where

-- Constants : variables, numbers, etc.
data Constant =
  Letter Char
  | Number Int
  | GreekLetter String
  | StdFun String
  -- Operation symbols
  | Add | Sub | Mul | Mmul | Mmmul | Sslash | Bbslash
  | Times | Div | Comp | Oplus | Otimes | Odot
  | Sum | Prod | Wedge | Wwedge | Vv | Vvv | Nn | Nnn | Uu | Uuu
  -- Miscellaneous symbols
  | Inte | Oint | Del | Grad | Addsub | Void | Infty | Aleph
  | Angle | Therefore | Abs | Cdots | Vdots | Ddots | Bslash
  | Quad | Diamond | Square | Lfloor | Rfloor | Lceil | Rceil
  | Cc | Ensnn | Qq | Rr | Zz
  -- Relation symbols
  | Eq | Neq | Lt | Gt | Le | Ge | Prec | Succ
  | In | Notin | Subset | Supset | Subsete | Supsete
  | Mod | Congr | Approx | Prop
  -- Logical symbols
  | And | Or | Not | Implies | If | Iff | Forall | Exists
  | Falsum | Taut | Turnstile | Tturnstile
  -- Arrows
  | Uarr | Darr | Larr | To
  | Mapsto | Harr | Llarr | Hharr
  deriving (Show)

-- Unary operators
data UnaryOp = 
  Usqrt | Utext
  | Ubb | Ubbb | Ucc | Utt | Ufr | Usf
  | Uhat | Ubar | Uul | Uvec | Udot | Uddot 
  deriving (Show)

-- Binary operators
data BinaryOp = BFrac | BRoot | BStackRel deriving (Show)

-- Left brackets
data LBracket = LPar | LCro | LBra | LChe | LBraCons deriving (Show)

-- Right brackets 
data RBracket = RPar | RCro | RBra | RChe | RBraCons deriving (Show)

-- Simple expressions
data SimpleExpr =
  SEConst Constant
  | Delimited LBracket Expr RBracket
  | UnaryApp UnaryOp SimpleExpr
  | BinaryApp BinaryOp SimpleExpr SimpleExpr
  | Raw String  -- raw text that cannot be interpreted 
  deriving(Show)

-- Global expressions
data Expr =
  Seq SimpleExpr Expr
  | Simple SimpleExpr
  | Frac SimpleExpr SimpleExpr
  | Under SimpleExpr SimpleExpr
  | Super SimpleExpr SimpleExpr
  | SubSuper SimpleExpr SimpleExpr SimpleExpr
  deriving (Show)

