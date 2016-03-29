module Ast where

-- Greek letters
data Greek = 
  Galpha | Gbeta |  Gchi | Gdelta | GDelta | Gepsilon | Gvarepsilon
  | Geta | Ggamma | GGamma | Giota | Gkappa | Glambda | GLambda
  | Gmu | Gnu | Gomega | GOmega | Gphi | GPhi | Gvarphi
  | Gpi | GPi | Gpsi | GPsi | Grho | Gsigma | GSigma | Gtau 
  | Gtheta | GTheta | Gvartheta | Gupsilon | Gxi | GXi | Gzeta
  deriving (Show)

-- Constants : variables, numbers, etc.
data Constant =
  Letter Char
  | Number Int
  | GreekLetter Greek
  -- TODO : A lot of other constants !
  deriving (Show)

-- Unary operators
data UnaryOp = 
  Usqrt | Utext
  | Ubb | Ubbb | Ucc | Utt | Ufr | Usf
  deriving (Show)

-- Binary operators
data BinaryOp = BFrac | BRoot | BStackrel deriving (Show)

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
  Simple SimpleExpr
  | Seq SimpleExpr Expr
  | Frac SimpleExpr SimpleExpr
  | Sub SimpleExpr SimpleExpr
  | Super SimpleExpr SimpleExpr
  | SubSuper SimpleExpr SimpleExpr SimpleExpr
  deriving (Show)
