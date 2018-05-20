module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-), ignore) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a, b) = a:b

(-#) :: Parser a -> Parser b -> Parser b
m -# n = (m # n) >-> snd

(#-) :: Parser a -> Parser b -> Parser a
m #- n = (m # n) >-> fst

spaces :: Parser String
spaces xs = return (takeWhile isSpace xs) (dropWhile isSpace xs)

token :: Parser a -> Parser a
token m = m #- spaces

letter :: Parser Char
letter [] = fail []
letter (x:xs) 
    | isAlpha x = return x xs 
    | otherwise = fail xs   

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String
chars n xs
  | n <= length xs = return (take n xs) (drop n xs)
  | otherwise = fail xs

ignore :: Parser String
ignore xs = return "" (dropWhile (/='\n') xs)

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

require :: String -> Parser String
require w = accept w ! err w

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

