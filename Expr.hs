module Expr(Expr, T, parse, fromString, value, toString) where

{-
   An expression of type Expr is a representation of an arithmetic expression 
   with integer constants and variables. A variable is a string of upper- 
   and lower case letters. The following functions are exported
   
   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int
   
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.
   
   fromString expects its argument to contain an expression and returns the 
   corresponding Expr. 
  
   toString converts an expression to a string without unneccessary 
   parentheses and such that fromString (toString e) = e.
  
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.  
-}
import Prelude hiding (return, fail, lookup)
import Parser hiding (T)
import Dictionary (lookup)
import qualified Dictionary

data Expr = Num Integer | Var String | Add Expr Expr 
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
       | Pow Expr Expr
         deriving Show

type T = Expr

var, num, factor, term, expr :: Parser Expr

term', expr' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> (\_ -> Mul) !
        lit '/' >-> (\_ -> Div)

addOp = lit '+' >-> (\_ -> Add) !
        lit '-' >-> (\_ -> Sub)

powOp = lit '^' >-> (\_ -> Pow)

bldOp e (oper,e') = oper e e'

factor = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"

pow' e = powOp # factor >-> bldOp e #> pow' ! return e
pow = factor #> pow'
             
term' e = mulOp # pow >-> bldOp e #> term' ! return e
term = pow #> term'
       
expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)
shw prec (Pow t u) = parens (prec>7) (shw 7 t ++ "^" ++ shw 8 u)

value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _ = n
value (Var n) d = case lookup n d of
  Nothing -> error ("Expr.value: undefined variable "++n++"\n")
  Just x -> x
value (Add t u) d = (value t d) + (value u d)
value (Sub t u) d = (value t d) - (value u d)
value (Mul t u) d = (value t d) * (value u d)
value (Div t u) d = case value u d of
  0 -> error "Expr.value: division by 0"
  x -> quot (value t d) x
value (Pow t u) d = (value t d) ^ (value u d)

instance Parse Expr where
    parse = expr
    toString = shw 0
