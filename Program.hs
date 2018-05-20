module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T]
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString = Statement.toString

shw :: [Statement.T] -> String
shw = concat . map Statement.toString

prnt :: [Statement.T] -> IO ()
prnt stmts = putStr $ shw stmts
             
exec = error "Program.exec not implemented"
