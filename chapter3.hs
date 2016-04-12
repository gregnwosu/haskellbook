module Print3 where

myGreeting :: String
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world"

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f (a,b) (c,d) =  ((b,d),(a,c))

main :: IO ()
main = do
   putStrLn myGreeting
   putStrLn secondGreeting
   where secondGreeting  = concat [hello, " ", world]
