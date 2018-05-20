module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving (Show)
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString = shw

shw :: T -> String
shw (Program stmts) = concatMap Statement.toString stmts

prnt :: T -> IO ()
prnt prgm = putStr $ shw prgm

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) = Statement.exec stmts Dictionary.empty
