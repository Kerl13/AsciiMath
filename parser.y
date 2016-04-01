{
module Parser (parseAscii) where
import Lexer
import Ast
}

%name parseAscii
%tokentype { Token }
%error { error . show }

%token
  RAW         { RAW _ }
  LETTER      { LETTER _ }
  NUM         { NUM _ }
  LDEL        { LDEL _ }
  RDEL        { RDEL _ }
  '/'         { SLASH }
  '_'         { UNDERSCORE }
  '^'         { SUPER }
  GREEK       { GREEK _ }
  STDFUN      { STDFUN _ }
  SQRT        { SQRT } 
  TEXT        { TEXT } 
  BB          { BB } 
  BBB         { BBB } 
  UCC         { UCC } 
  TT          { TT } 
  FR          { FR } 
  SF          { SF }
  FRAC        { FRAC } 
  ROOT        { ROOT } 
  STACKREL    { STACKREL }
  ADD         { ADD } 
  SUB         { SUB } 
  MUL         { MUL } 
  MMUL        { MMUL } 
  MMMUL       { MMMUL } 
  SSLASH      { SSLASH } 
  BBSLASH     { BBSLASH }
  TIMES       { TIMES } 
  DIV         { DIV } 
  COMP        { COMP } 
  OPLUS       { OPLUS } 
  OTIMES      { OTIMES } 
  ODOT        { ODOT }
  SUM         { SUM } 
  PROD        { PROD } 
  WEDGE       { WEDGE } 
  WWEDGE      { WWEDGE } 
  VV          { VV } 
  VVV         { VVV } 
  NN          { NN } 
  NNN         { NNN } 
  UU          { UU } 
  UUU         { UUU }
  INT         { INT } 
  OINT        { OINT } 
  DEL         { DEL } 
  GRAD        { GRAD } 
  ADDSUB      { ADDSUB } 
  VOID        { VOID } 
  INFTY       { INFTY } 
  ALEPH       { ALEPH }
  ANGLE       { ANGLE } 
  THEREFORE   { THEREFORE } 
  ABS         { ABS } 
  CDOTS       { CDOTS } 
  VDOTS       { VDOTS } 
  DDOTS       { DDOTS } 
  BSLASH      { BSLASH }
  QUAD        { QUAD } 
  DIAMOND     { DIAMOND } 
  SQUARE      { SQUARE } 
  LFLOOR      { LFLOOR } 
  RFLOOR      { RFLOOR } 
  LCEIL       { LCEIL } 
  RCEIL       { RCEIL }
  CC          { CC } 
  ENSNN       { ENSNN } 
  QQ          { QQ } 
  RR          { RR } 
  ZZ          { ZZ }
  EQ          { Lexer.EQ } 
  NEQ         { NEQ } 
  LT          { Lexer.LT } 
  GT          { Lexer.GT } 
  LE          { Lexer.LE } 
  GE          { Lexer.GE } 
  PREC        { PREC } 
  SUCC        { SUCC }
  IN          { IN } 
  NOTIN       { NOTIN } 
  SUBSET      { SUBSET } 
  SUPSET      { SUPSET } 
  SUBSETE     { SUBSETE } 
  SUPSETE     { SUPSETE }
  MOD         { MOD } 
  CONGR       { CONGR } 
  APPROX      { APPROX } 
  PROP        { PROP }
  AND         { AND } 
  OR          { OR } 
  NOT         { NOT } 
  IMPLIES     { IMPLIES } 
  IF          { IF } 
  IFF         { IFF } 
  FORALL      { FORALL } 
  EXISTS      { EXISTS }
  FALSUM      { FALSUM } 
  TAUT        { TAUT } 
  TURNSTILE   { TURNSTILE } 
  TTURNSTILE  { TTURNSTILE }
  UARR        { UARR } 
  DARR        { DARR } 
  LARR        { LARR } 
  TO          { TO }
  MAPSTO      { MAPSTO } 
  HARR        { HARR } 
  LLARR       { LLARR } 
  HAT         { HAT } 
  BAR         { BAR } 
  UL          { UL } 
  VEC         { VEC } 
  DOT         { DOT } 
  DDOT        { DDOT } 
  COMMA       { COMMA }


%%

code:
    expr        { [$1] }
    | expr code { $1:$2 }

expr:
    simpleExpr                                  { Simple $1 }
    | simpleExpr '/' simpleExpr                 { Frac $1 $3 }
    | simpleExpr '_' simpleExpr                 { Under $1 $3 }
    | simpleExpr '^' simpleExpr                 { Super $1 $3 }
    | simpleExpr '_' simpleExpr '^' simpleExpr  { SubSuper $1 $3 $5 }

const:
    LETTER        { let LETTER s = $1 in Letter s }
    | NUM         { let NUM n = $1 in Number n }
    | GREEK       { let GREEK s = $1 in GreekLetter s }
    | STDFUN      { let STDFUN s = $1 in StdFun s }
    -- Operation symbols
    | ADD         { Add }
    | SUB         { Sub }
    | MUL         { Mul }
    | MMUL        { Mmul }
    | MMMUL       { Mmmul }
    | SSLASH      { Sslash }
    | BBSLASH     { Bbslash }
    | TIMES       { Times }
    | DIV         { Div }
    | COMP        { Comp }
    | OPLUS       { Oplus } 
    | OTIMES      { Otimes }
    | ODOT        { Odot }
    | SUM         { Sum }
    | PROD        { Prod }
    | WEDGE       { Wedge }
    | WWEDGE      { Wwedge }
    | VV          { Vv }
    | VVV         { Vvv }
    | NN          { Nn }
    | NNN         { Nnn }
    | UU          { Uu }
    | UUU         { Uuu }    
    -- Miscellaneous symbols
    | INT         { Inte }
    | OINT        { Oint }
    | DEL         { Del }
    | GRAD        { Grad }
    | ADDSUB      { Addsub }
    | VOID        { Void }
    | INFTY       { Infty }
    | ALEPH       { Aleph }
    | ANGLE       { Angle }
    | THEREFORE   { Therefore }
    | ABS         { Abs }
    | CDOTS       { Cdots } 
    | VDOTS       { Vdots }
    | DDOTS       { Ddots }
    | BSLASH      { Bslash }
    | QUAD        { Quad }
    | DIAMOND     { Diamond }
    | SQUARE      { Square }
    | LFLOOR      { Lfloor }
    | RFLOOR      { Rfloor }
    | LCEIL       { Lceil }
    | RCEIL       { Rceil }
    | CC          { Cc }
    | ENSNN       { Ensnn }
    | QQ          { Qq }
    | RR          { Rr }
    | ZZ          { Zz }
    -- Relation symbols
    | EQ          { Eq }
    | NEQ         { Neq }
    | LT          { Lt }
    | GT          { Gt }
    | LE          { Le }
    | GE          { Ge }
    | PREC        { Prec }
    | SUCC        { Succ }
    | IN          { In }
    | NOTIN       { Notin }
    | SUBSET      { Subset }
    | SUPSET      { Supset }
    | SUBSETE     { Subsete }
    | SUPSETE     { Supsete }
    | MOD         { Mod }
    | CONGR       { Congr } 
    | APPROX      { Approx }
    | PROP        { Prop }
    -- Logical symbols
    | AND         { And }
    | OR          { Or }
    | NOT         { Not }
    | IMPLIES     { Implies }
    | IF          { If }
    | IFF         { Iff }
    | FORALL      { Forall }
    | EXISTS      { Exists }
    | FALSUM      { Falsum }
    | TAUT        { Taut }
    | TURNSTILE   { Turnstile }
    | TTURNSTILE  { Tturnstile }
    -- Arrows
    | UARR        { Uarr }
    | DARR        { Darr }
    | LARR        { Larr }
    | TO          { To }
    | MAPSTO      { Mapsto }
    | HARR        { Harr }
    | LLARR       { Llarr }
    | COMMA       { Comma }

op1:
    SQRT        { Usqrt }
    | TEXT      { Utext }
    | BB        { Ubb }
    | BBB       { Ubbb }
    | UCC        { Ucc }
    | TT        { Utt }
    | FR        { Ufr }
    | SF        { Usf }
    | HAT       { Uhat }
    | BAR       { Ubar }
    | UL        { Uul }
    | VEC       { Uvec }
    | DOT       { Udot }
    | DDOT      { Uddot }

op2:
    FRAC        { BFrac }
    | ROOT      { BRoot }
    | STACKREL  { BStackRel }

lDel : LDEL   { let LDEL s = $1 in ldel s }

rDel : RDEL   { let RDEL s = $1 in rdel s }

simpleExpr:
    const                       { SEConst $1 }
    | lDel code rDel            { Delimited $1 $2 $3 }
    | op1 simpleExpr            { UnaryApp $1 $2 }
    | op2 simpleExpr simpleExpr { BinaryApp $1 $2 $3 }
    | RAW                       { let RAW s = $1 in Raw s }

{

rdel ")" = RPar
rdel "]" = RCro
rdel "}" = RBra
rdel ":)" = RChe
rdel ":}" = RBraCons

ldel "(" = LPar
ldel "[" = LCro
ldel "{" = LBra
ldel "(:" = LChe
ldel "{:" = LBraCons

}
