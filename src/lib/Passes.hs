module Passes where
import Ast

------------------------
-- Recognise matrices --
------------------------

-- main function
matrix :: Code -> Code
matrix = map matrixExpr

matrixExpr :: Expr -> Expr
matrixExpr (Simple e) = Simple $ matrixSE e
matrixExpr (Frac e1 e2) = Frac (matrixSE e1) (matrixSE e2)
matrixExpr (Under e1 e2) = Under (matrixSE e1) (matrixSE e2)
matrixExpr (Super e1 e2) = Super (matrixSE e1) (matrixSE e2)
matrixExpr (SubSuper e1 e2 e3) = SubSuper (matrixSE e1) (matrixSE e2) (matrixSE e3)

matrixSE :: SimpleExpr -> SimpleExpr
matrixSE (Delimited LCro c RCro) = case parseSeq c of
    Nothing -> Delimited LCro (matrix c) RCro
    Just m -> Matrix RawMatrix m
matrixSE (Delimited LPar c RPar) = case parseSeq c of
    Nothing -> Delimited LPar (matrix c) RPar
    Just m -> Matrix ColMatrix m
matrixSE (UnaryApp o e) = UnaryApp o (matrixSE e)
matrixSE (BinaryApp o e1 e2) = BinaryApp o (matrixSE e1) (matrixSE e2)
matrixSE x = x

-- Usefull constant
comma :: Expr
comma = Simple (SEConst Comma)

-- like unwords but cuts at x
split :: Eq a => a -> [a] -> [[a]]
split x l = case span ((/=) x) l of
    (_, []) -> [l]
    (l1, l2) -> l1 : (split x $ drop 1 l2)


-- First step of parseSeq : match a sequence of comma-separated bracketed
-- expression
unbracket :: [Code] -> Maybe [(LBracket, Code)] 
unbracket [] = Just []
unbracket ([(Simple (Delimited lb c rb))]:cs) = case (lb, rb, unbracket cs) of
    (LCro, RCro, Just l) -> Just ((LCro, c):l)
    (LPar, RPar, Just l) -> Just ((LPar, c):l)
    _ -> Nothing 
unbracket _ = Nothing

parseSeq1 :: Code -> Maybe [(LBracket, Code)]
parseSeq1 = unbracket . split comma

-- Second step of parseSeq : check if all the delimiters used are similars
parseSeq2 :: Maybe [(LBracket, Code)] -> Maybe [[Code]]
parseSeq2 Nothing = Nothing
parseSeq2 (Just cs) =
  let (lb, _) = head cs in
  if all (\(lb', _) -> lb' == lb) cs then
    let res = map ((split comma) . snd) cs in
    let n = length . head $ res in
    if all (\l -> n == length l) res then
      Just res
      else
        Nothing    
  else
    Nothing

-- ParseSeq
parseSeq :: Code -> Maybe [[Code]]
parseSeq = parseSeq2 . parseSeq1

