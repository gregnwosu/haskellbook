module Sing where

fstString :: [Char] ->  [Char]
fstString x = x ++ " in the rain"
sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"
sing  y = if (x > y) then fstString x else sndString y
            where x = "Singin"
x = "Somewhere"


main :: IO ()
main = do
  print (1 + 2)
  putStrLn (show 10)
  print $ show (negate (-1::Int))
  print ((+) 0 blah)
    where blah = negate 1
