module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail, read)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Begin Statement
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";"
  >-> \(v, e) -> Assignment v e

myIf = accept "if" -# Expr.parse #-
  require "then" # parse #-
  require "else" # parse
  >-> \((x, y), z) -> If x y z

skip = accept "skip" # require ";"
  >-> \_ -> Skip

while = accept "while" -# Expr.parse #- require "do" # parse
  >-> \(x, y) -> While x y

read = accept "read" -# word #- require ";"
  >-> \x -> Read x

write = accept "write" -# Expr.parse #- require ";"
  >-> \x -> Write x

-- TODO: multiple statements
begin = accept "begin" -# parse #- require "end"
  >-> \x -> Begin x

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
  if (Expr.value cond dict)>0 
  then exec (thenStmts: stmts) dict input
  else exec (elseStmts: stmts) dict input
exec (Assignment name expr: stmts) dict input = exec stmts nextDict input
  where nextDict = Dictionary.insert (name, Expr.value expr dict) dict
exec (Skip: stmts) dict input = exec stmts dict input
exec (While cond block: stmts) dict input =
  if (Expr.value cond dict)>0 
  then exec (block: (While cond block): stmts) dict input
  else exec stmts dict input
exec (Read name: stmts) dict (i:input) = exec stmts nextDict input
  where nextDict = Dictionary.insert (name, i) dict
exec (Write expr: stmts) dict input = (Expr.value expr dict):(exec stmts dict input)
exec (Begin statement:stmts) dict input = exec (statement:stmts) dict input


instance Parse Statement where
  parse = assignment ! myIf ! skip ! while ! read ! write ! begin
  toString = error "Statement.toString not implemented"
