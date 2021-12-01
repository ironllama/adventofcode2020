import Prelude hiding(interact)
import qualified Prelude

import Text.Parsec hiding(count, parse)
import qualified Text.Parsec as Parsec

interact :: Show a => ([String] -> a) -> IO ()
interact f = Prelude.interact $ (++"\n") . show . f . lines

type Parser = Parsec String ()

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

count :: Eq a => a -> [a] -> Int
count c = length . filter (==c)

rights :: [Either a b] -> [b]
rights (Right x:xs) = x:rights xs
rights (_:xs) = rights xs
rights [] = []


main = interact $ f . rights . map (parse p)

p :: Parser (Int, Int, Char, String)
p = do
  low <- many1 digit
  char '-'
  high <- many1 digit
  char ' '
  c <- letter
  string ": "
  s <- many1 letter
  return $ (read low, read high, c, s)

f xs = count True $ map test xs

test (low, high, c, s) = let n = count c s
                         in  n >= low && n <= high
