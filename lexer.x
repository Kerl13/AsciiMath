{
module Lexer (get_tokens, Token(..)) where
import qualified Data.Map as M
import qualified Data.Set as S
}

%wrapper "posn"

$alpha = [a-zA-Z]
$digit = [0-9]

@ldel = "(" | "[" | "{" | "(:" | "{:"
@rdel = ")" | "]" | "}" | ":)" | ":}"
@sym1 = "+" | "*" | "-" | "/" | "@" | "<" | ">" | "|" | "," | \. | \\ | \^
      | = | \_

@ident = $alpha+

tokens :-
  $white+     ;
  @ident      { \_ s -> check_kw s }
  $digit+     { \_ s -> NUM (read s) }
  @ldel       { \_ s -> LDEL s }
  @rdel       { \_ s -> RDEL s }
  @sym1       { \_ s -> check_sym1 s }
  "+-"        { cst ADDSUB }
  "**"        { cst MMUL }
  "//"        { cst SSLASH }
  \\\\        { cst BBSLASH }
  "-:"        { cst DIV }
  "o+"        { cst OPLUS }
  "o."        { cst ODOT }
  \^\^        { cst WEDGE }
  "O/"        { cst VOID }
  "/_"        { cst ANGLE }
  ":."        { cst THEREFORE }
  "|~"        { cst LCEIL }
  "~|"        { cst RCEIL }
  "!="        { cst NEQ }
  "<="        { cst LE }
  ">="        { cst GE }
  "-<"        { cst PREC }
  ">-"        { cst SUCC }
  "-="        { cst MOD }
  "~="        { cst CONGR }
  "~~"        { cst APPROX }
  "=>"        { cst IMPLIES }
  "->"        { cst TO }
  "***"       { cst MMMUL }
  \^\^\^      { cst WWEDGE }
  "|__"       { cst LFLOOR }
  "__|"       { cst RFLOOR }
  "!in"       { cst NOTIN }
  "_|_"       { cst FALSUM }
  "|--"       { cst TURNSTILE }
  "|=="       { cst TTURNSTILE }

{
data Token =
  RAW String
  | LETTER Char | LETTERS_ String -- Temporary token
  | NUM Int
  | LDEL String
  | RDEL String
  | SLASH | UNDERSCORE | SUPER
  -- Greek letters
  | GREEK String 
  -- Standard functions
  | STDFUN String
  -- Unary ops
  | SQRT | TEXT | BB | BBB | UCC | TT | FR | SF
  --Binary ops
  | FRAC | ROOT | STACKREL
  -- Operation symbols
  | ADD | SUB | MUL | MMUL | MMMUL | SSLASH | BBSLASH
  | TIMES | DIV | COMP | OPLUS | OTIMES | ODOT
  | SUM | PROD | WEDGE | WWEDGE | VV | VVV | NN | NNN | UU | UUU
  -- Miscellaneous symbols
  | INT | OINT | DEL | GRAD | ADDSUB | VOID | INFTY | ALEPH
  | ANGLE | THEREFORE | ABS | CDOTS | VDOTS | DDOTS | BSLASH
  | QUAD | DIAMOND | SQUARE | LFLOOR | RFLOOR | LCEIL | RCEIL
  | CC | ENSNN | QQ | RR | ZZ
  -- Relation symbols
  | EQ | NEQ | LT | GT | LE | GE | PREC | SUCC
  | IN | NOTIN | SUBSET | SUPSET | SUBSETE | SUPSETE
  | MOD | CONGR | APPROX | PROP
  -- Logical symbols
  | AND | OR | NOT | IMPLIES | IF | IFF | FORALL | EXISTS
  | FALSUM | TAUT | TURNSTILE | TTURNSTILE
  -- Arrows
  | UARR | DARR | LARR | TO
  | MAPSTO | HARR | LLARR
  -- Accents
  | HAT | BAR | UL | VEC | DOTOP | DDOT 
  -- Additionnal tokens 
  | COMMA | DOT
  deriving (Show)

cst :: t -> (AlexPosn -> String -> t)
cst x = (\_ _ -> x)

kws :: M.Map String Token
kws = M.fromList [
  -- Unary ops
  ("sqrt", SQRT), ("text", TEXT),
  ("bb", BB),     ("bbb", BBB),  ("cc", CC),
  ("tt", TT),     ("fr", FR),    ("sf", SF),
  -- Binary ops
  ("frac", FRAC),
  ("root", ROOT),
  ("stackrel", STACKREL),
  -- Operation symbols
  ("xx", TIMES), ("ox", OTIMES), ("sum", SUM), ("prod", PROD),
  ("vv", VV), ("vvv", VVV), ("nn", NN), ("nnn", NNN), ("uu", UU), ("uuu", UUU),
  -- Miscellaneous symbols
  ("int", INT), ("oint", OINT), ("del", DEL), ("grad", GRAD),
  ("oo", INFTY), ("aleph", ALEPH),
  ("cdots", CDOTS), ("vdots", VDOTS), ("ddots", DDOTS),
  ("quad", QUAD), ("diamond", DIAMOND), ("square", SQUARE),
  ("CC", CC), ("NN", ENSNN), ("QQ", QQ), ("RR", RR), ("ZZ", ZZ),
  -- Relation symbols
  ("in", IN), ("sub", SUBSET), ("sup", SUPSET),
  ("sube", SUBSETE), ("supe", SUPSETE), ("prop", PROP),
  -- Logical symbols
  ("and", AND), ("or", OR), ("not", NOT), ("if", IF), ("iff", IFF),
  ("AA", FORALL), ("EE", EXISTS), ("TT", TAUT),
  -- Arrows
  ("uarr", UARR), ("darr", DARR), ("rarr", TO), ("larr", LARR),
  ("rArr", IMPLIES), ("lArr", LLARR), ("hArr", IFF),
  -- Accents
  ("hat", HAT), ("bar", BAR), ("ul", UL),
  ("vec", VEC), ("dot", DOTOP), ("ddot", DDOT)]

greek_letters :: S.Set String
greek_letters = S.fromList [
  "alpha", "beta", "chi", "delta", "Delta",
  "epsilon", "varepsilon", "eta", "gamma", "Gamma",
  "iota", "kappa", "lambda", "Lambda", "mu", "nu",
  "omega", "Omega", "phi", "Phi", "varphi", "pi", "Pi",
  "psi", "Psi", "rho", "sigma", "Sigma", "tau", "theta", "Theta",
  "vartheta", "upsilon", "xi", "Xi", "zeta"]

std_fun :: S.Set String
std_fun = S.fromList [
  "sin", "cos", "tan", "csc", "sec", "cot",
  "sinh", "cosh", "tanh", "log", "ln", "exp", "det", "dim", "lim", "mod",
  "gcd", "lcm", "min", "max"]

check_kw :: String -> Token
check_kw s = case M.lookup s kws of
    Just tok -> tok
    Nothing ->
        if S.member s greek_letters then
          GREEK s
        else if S.member s std_fun then
            STDFUN s
          else
            LETTERS_ s

sym1 :: M.Map String Token
sym1 = M.fromList [
  ("+", ADD), ("-", SUB), ("*", MUL), ("\\", BSLASH), ("/", SLASH),
  ("@", COMP), ("|", ABS), ("_", UNDERSCORE), ("^", SUPER), 
  ("=", Lexer.EQ), ("<", Lexer.LT), (">", Lexer.GT), (",", COMMA), 
  (".", DOT), ("\\", BSLASH), ("^", SUPER), ("=", Lexer.EQ), ("_", UNDERSCORE)]

check_sym1 :: String -> Token
check_sym1 s = case M.lookup s sym1 of
    Just tok -> tok
    Nothing -> error ("'" ++ s ++ "' is supposed to be recognised")

split :: String -> Either String (String, String, String)
split s = case span ((/=) '"') s of
    (_, "")   -> Left s
    (a, _:t)  ->
        case span ((/=) '"') t of
            (_, "")   -> error "Unterminated string"
            (b, _:c)  -> Right (a, b, c)

get_tokens_ :: String -> [Token]
get_tokens_ s = case split s of
    Left s' -> alexScanTokens s'
    Right (prev, str, next) ->
        (alexScanTokens prev) ++ [RAW str] ++ (get_tokens_ next)

get_tokens :: String -> [Token]
get_tokens = unletters . get_tokens_
    where unletters [] = []
          unletters ((LETTERS_ s):toks) = (map LETTER s) ++ (unletters toks)
          unletters (tok:toks) = tok:(unletters toks)
}

