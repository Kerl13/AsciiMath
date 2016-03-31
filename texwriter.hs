module TeXWriter (writeTeX) where
import Ast

cmd :: String -> String
cmd s = "\\" ++ s

cmdarg :: String -> String -> String
cmdarg c a = cmd c ++ "{" ++ a ++ "}"
cmdarg2 c a1 a2 = cmd c ++ "{" ++ a1 ++ "}{" ++ a2 ++ "}"

writeConst :: Constant -> String
-- Operation symbols
writeConst (Letter c) = [c]
writeConst (Number n) = show n
writeConst (GreekLetter s) = cmd s
writeConst (StdFun s) = cmd s
writeConst Add = "+"
writeConst Sub = "-"
writeConst Mul = cmd "cdot"
writeConst Mmul = cmd "ast"
writeConst Mmmul = cmd "star"
writeConst Sslash = "/"
writeConst Bbslash = cmd "\\"
writeConst Times = cmd "times"
writeConst Div = cmd "div"
writeConst Comp = cmd "circ"
writeConst Oplus = cmd "oplus"
writeConst Otimes = cmd "otimes"
writeConst Odot = cmd "odot"
writeConst Sum = cmd "sum"
writeConst Prod = cmd "prod"
writeConst Wedge = cmd "wedge"
writeConst Wwedge = cmd "bigwedge"
writeConst Vv = cmd "vee"
writeConst Vvv = cmd "bigvee"
writeConst Nn = cmd "cap"
writeConst Nnn = cmd "bigcap"
writeConst Uu = cmd "cup"
writeConst Uuu = cmd "bigcup"
-- Miscellaneous symbols
writeConst Inte = cmd "int"
writeConst Oint = cmd "oint"
writeConst Del = cmd "partial"
writeConst Grad = cmd "nabla"
writeConst Addsub = cmd "pm"
writeConst Void = cmd "emptyset"
writeConst Infty = cmd "infty"
writeConst Aleph = cmd "aleph"
writeConst Angle = cmd "angle"
writeConst Therefore = cmd "therefore"
writeConst Abs = "|"
writeConst Cdots = cmd "cdots"
writeConst Vdots = cmd "vdots"
writeConst Ddots = cmd "ddots"
writeConst Bslash = "\\"
writeConst Quad = cmd "quad"
writeConst Diamond = cmd "diamond"
writeConst Square = cmd "square"
writeConst Lfloor = cmd "lfloor"
writeConst Rfloor = cmd "rfloor"
writeConst Lceil = cmd "lceil"
writeConst Rceil = cmd "rceil"
writeConst Cc = cmdarg "mathbb" "C"
writeConst Ensnn = cmdarg "mathbb" "N"
writeConst Qq = cmdarg "mathbb" "Q"
writeConst Rr = cmdarg "mathbb" "R"
writeConst Zz = cmdarg "mathbb" "Z"
-- Relation symbols
writeConst Eq = "="
writeConst Neq = cmd "neq"
writeConst Lt = "<"
writeConst Gt = ">"
writeConst Le = cmd "leqslant"
writeConst Ge = cmd "gesqlant"
writeConst Prec = cmd "prec"
writeConst Succ = cmd "succ"
writeConst In = cmd "in"
writeConst Notin = cmd "not" ++ cmd "in"
writeConst Subset = cmd "subset"
writeConst Supset = cmd "supset"
writeConst Subsete = cmd "subseteq"
writeConst Supsete = cmd "supseteq"
writeConst Mod = cmd "equiv"
writeConst Congr = cmd "cong"
writeConst Approx = cmd "approx"
writeConst Prop = cmd "propto"
-- Logical symbols
writeConst And = cmdarg "textrm" "and"
writeConst Or = cmdarg "textrm" "or"
writeConst Not = cmd "neg"
writeConst Implies = cmd "Rightarrow"
writeConst If = cmdarg "textrm" "if"
writeConst Iff = cmd "LeftRightarrow"
writeConst Forall = cmd "forall"
writeConst Exists = cmd "exists"
writeConst Falsum = cmd "perp"
writeConst Taut = cmd "top"
writeConst Turnstile = cmd "vdash"
writeConst Tturnstile = cmd "models"
-- Arrows
writeConst Uarr = cmd "uparrow"
writeConst Darr = cmd "downarrow"
writeConst Larr = cmd "leftarrow"
writeConst To = cmd "to"
writeConst Mapsto = cmd "mapsto"
writeConst Harr = cmd "leftrightarrow"
writeConst Llarr = cmd "Leftarrow"

-- Writes a unary operator
writeUnaryOp :: UnaryOp -> String
writeUnaryOp Usqrt = "sqrt"
writeUnaryOp Utext = "textrm"
writeUnaryOp Ubb = "mathbf"
writeUnaryOp Ubbb = "mathbb"
writeUnaryOp Ucc = "mathcal"
writeUnaryOp Utt = "texttt"
writeUnaryOp Ufr = "mathfrac"
writeUnaryOp Usf = "mathsf"
writeUnaryOp Uhat = "hat"
writeUnaryOp Ubar = "overline"
writeUnaryOp Uul = "underline"
writeUnaryOp Uvec = "vec"
writeUnaryOp Udot = "dot"
writeUnaryOp Uddot = "ddot"

-- Writes the delimitors
writeLBracket :: LBracket -> String
writeRBracket :: RBracket -> String
writeLBracket l = cmd "left" ++ aux l
    where aux LPar = "("
          aux LCro = "["
          aux LBra = "\\{"
          aux LChe = cmd "langle"
          aux LBraCons = "."
writeRBracket r = cmd "right" ++ aux r 
   where  aux RPar = ")"
          aux RCro = "]"
          aux RBra = "\\}"
          aux RChe = cmd "rangle"
          aux RBraCons = "."

-- Writes a simple expression
writeSimpleExpr :: SimpleExpr -> String
writeSimpleExpr (SEConst c) = writeConst c
writeSimpleExpr (Delimited l e r) = 
    writeLBracket l ++ writeCode e ++ writeRBracket r
writeSimpleExpr (UnaryApp o e) =
    cmdarg (writeUnaryOp o) (writeSimpleExpr e)
writeSimpleExpr (BinaryApp BFrac e1 e2) =
    cmdarg2 "frac" (writeSimpleExpr e1) (writeSimpleExpr e2)
writeSimpleExpr (BinaryApp BRoot e1 e2) =
    cmdarg ("sqrt[" ++ writeSimpleExpr e1 ++ "]") $ writeSimpleExpr e2
writeSimpleExpr (BinaryApp BStackRel e1 e2) =
    cmdarg2 "stackrel" (writeSimpleExpr e1) (writeSimpleExpr e2)
writeSimpleExpr (Raw s) = cmdarg "textrm" s

-- Writes a simple expression after removing the delimiters at the embracing
-- delimiters if present
writeSimpleExprND :: SimpleExpr -> String
writeSimpleExprND (Delimited _ e _) = writeCode e
writeSimpleExprND e = writeSimpleExpr e

-- Writes an expression
writeExpr :: Expr -> String
writeExpr (Simple se) = writeSimpleExpr se
writeExpr (Frac e1 e2) =
    cmdarg2 "frac" (writeSimpleExprND e1) (writeSimpleExprND e2)
writeExpr (Under e1 e2) =
    writeSimpleExpr e1 ++ "_{" ++ writeSimpleExprND e2 ++ "}"
writeExpr (Super e1 e2) = 
    writeSimpleExpr e1 ++ "^{" ++ writeSimpleExprND e2 ++ "}"
writeExpr (SubSuper e1 e2 e3) =
    writeSimpleExpr e1 ++
    "_{" ++ writeSimpleExprND e2 ++ "}" ++
    "^{" ++ writeSimpleExprND e3 ++ "}"

-- Writes a code block
writeCode :: Code -> String
writeCode = foldr (\e s ->writeExpr e ++ s) ""

-- The main writer
writeTeX :: Code -> String
writeTeX = writeCode

