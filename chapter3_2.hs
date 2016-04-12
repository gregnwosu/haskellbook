module GlobalLocal where


topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
                     where
                       woot = 10

topLevelValue :: Integer
topLevelValue = 5
