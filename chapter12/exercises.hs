module Exercises where


replaceThe :: String -> String
replaceThe [] = []
replaceThe ('t':'h':'e':xs) = 'a':replaceThe xs
replaceThe (x:xs) = x:replaceThe xs

notThe :: String -> Maybe String
notThe [] = Just []
notThe ('t':'h':'e':xs) = Nothing
notThe (x:xs) = (:) <$> Just x <*> notThe xs

vowels = "aeiou"
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = fst . foldr go (0, False) .  words
  where
    go "the" (n,True) = (n+1, False)
    go "the" (n,False)  = (n,False)
    go [] t = t
    go (x:_) (n,_) = (n,x `elem` vowels)

countVowels :: String -> Integer
countVowels = foldr go 0
   where
     go l n
      | l `elem` vowels = n + 1
      | otherwise = n


newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = mkWord' (nv > nc)
 where
  nv = countVowels s
  nc = fromIntegral ( length s) - nv
  mkWord' True = Nothing
  mkWord' _ = Just $ Word' s


data Nat =
  Zero
  | Succ Nat deriving (Eq, Show)


natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | x == 0 = Just Zero
  | otherwise =   go (integerToNat $ x - 1)
    where go Nothing = Nothing
          go (Just n) = Just $ Succ n

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b ) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe a = mayybee a id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe l = Just $ head l

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just n) = [n]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr go []
 where go Nothing = id
       go (Just x ) = (x:)


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr go $ Just []
 where go Nothing _ = Nothing
       go _ Nothing = Nothing
       go (Just x) (Just xs) = Just $ x:xs


lefts' :: [Either a b] -> [a]
lefts'  = foldr go []
 where go (Right _ ) = id
       go (Left x) = (x:)

rights' :: [Either a b] -> [b]
rights'  = foldr go []
 where go (Left _ ) = id
       go (Right x) = (x:)

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr go ([],[])
 where go (Left a) (as, bs)  = (a:as, bs)
       go (Right b) (as, bs)  = (as, b:bs)

eitherMaybe' ::  (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just $ f x

either'  :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a: myIterate f (f a)

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f   = maybe [] (\(x,y) -> x : myUnfoldr f y) . f

betterIterate :: (a -> a) -> a -> [a]
betterIterate f  a = a : myUnfoldr (\ y -> Just (f y, f y)) a

data BinaryTree a =
  Leaf |
  Node (BinaryTree a) a (BinaryTree a) deriving (Show, Ord, Eq)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f  = maybe Leaf (\(l,b,r) -> Node (unfold f l) b (unfold f r) ) . f

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold go 0
  where
    go x
        | x == n = Nothing
        | otherwise = Just (x+1,x,x+1)
