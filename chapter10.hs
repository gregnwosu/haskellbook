module Chapter10 where

stops = "pbtdkg"
vowels = "aeiou"

nouns :: [String]
nouns = ["fox", "dog", "wall", "cow",  "fiddle", "moon" , "cat", "dish", "spoon"]
verbs :: [String]
verbs = ["jumped", "laugh", "run"]
svs :: [Char] -> [b] -> [(Char,b,Char)]
svs s v = [(x,y,z) | x <- s, y <- v, z <-s, x=='p']

seekritFunc :: Fractional a => String -> a
seekritFunc x =  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem = myAny . (==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr ff []
  where
    ff a = f' (f a) a
    f' True = (:)
    f' False = flip const

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs)= foldr (\ a b -> f' (f a b) a b) x xs
   where f' GT = const
         f' _  = flip const
myMinimumBy ::  (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myMaximumBy (flip f)
