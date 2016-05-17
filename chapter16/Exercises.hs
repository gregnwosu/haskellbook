{-# LANGUAGE RankNTypes #-}
module Exercises where
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Function

a_ = fmap (+1) $ read "[1]"::[Int]
b_ = (fmap.fmap) (++ "lol") (Just ["Hi", "Hello"])
c_ = fmap (*2) (\x -> x -2)
d_ = fmap ((return '1' ++ ) . show) (\x -> [x, 1..3])
e_ :: IO Integer
e_ =
  let
  ioi = readIO "1":: IO Integer
  changed = fmap (read . ("123" ++) .  show)  ioi
  in  fmap (*3) changed


functorCompose' :: (Eq (f c), Functor f) =>
    f a
 -> Fun a b
 -> Fun b c
 -> Bool
functorCompose' x (Fun _ a2b) (Fun _ b2c) =
  fmap b2c  (fmap a2b x) == fmap (b2c . a2b) x
type IntToString = Fun Int String
type StringToChar = Fun String Char

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftA Identity arbitrary

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

data Pair a = Pair a a
  deriving (Eq, Show)

data Three a b c = Three a b c
  deriving (Eq , Show)

data Four a b c d= Four a b c d
  deriving (Eq , Show)

newtype Three' a b c = Three' (Three a b c)
  deriving (Eq, Show)

newtype Four' a b c d= Four' (Four a b c d)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Pair a)
  where
    arbitrary = do
      a  <- arbitrary
      a' <- arbitrary
      return $ Pair a a'

instance Functor Pair where
  fmap f (Pair a b) = Pair (f b) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)
  where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return $ Two a b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)
  where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three' a b c)
  where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three' (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d)
  where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four' a b c d)
  where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four' (Four a b c d)






data Two a b = Two a b
  deriving (Show ,Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Functor (Three' a b) where
  fmap f (Three' three) = Three'$ fmap f three

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

instance Functor (Four' a b c) where
  fmap f (Four' four) = Four'$ fmap f four


main = do
  quickCheck (functorCompose' :: Identity Int -> IntToString -> StringToChar -> Bool)
  quickCheck (functorCompose' :: Pair Int  -> IntToString -> StringToChar -> Bool)
  quickCheck (functorCompose' :: Two Int Int -> IntToString -> StringToChar -> Bool)
  quickCheck (functorCompose' :: Three Int String Int -> IntToString -> StringToChar -> Bool)
  quickCheck (functorCompose' :: Three' Int String Int -> IntToString -> StringToChar -> Bool)
  quickCheck (functorCompose' :: Four Int String Int Int -> IntToString -> StringToChar -> Bool)
  quickCheck (functorCompose' :: Four' Int String Int Int -> IntToString -> StringToChar -> Bool)

data Possibly a = LolNope | Yeppers a
  deriving (Eq,Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers $ f a
  fmap _ _ = LolNope

data Sum a b = First a | Second b deriving (Eq , Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second $ f b

newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v

data Wrap f a = Wrap (f a)
  deriving (Eq, Show)

instance Functor x => Functor (Wrap x) where
  fmap f (Wrap x) = Wrap (fmap f x)

getInt :: IO Int
getInt = fmap read getLine

meeTooIsm :: IO String
meeTooIsm = do
  input <- getLine
  return (input ++ "and me too")

bumpIt :: IO Int
bumpIt = do
  intVal <- getInt
  return (intVal + 1)

--{-# LANGUAGE RankNTypes  #-}
--nat :: (f -> g) -> f a -> g a
--nat = undefined

type Nat f g = forall a. f a -> g a

g :: Nat f g
g = undefined

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- degenerateMtl :: Nat Maybe []
-- degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [ a + 1]

data BoolAndSomethingElse a =
  False' a  | True' a

instance Functor BoolAndSomethingElse where
  fmap f (True' a) = True' $ f a
  fmap f (False' a) = False' $ f a

data BoolAndMaybeSomethingElse a =
  Falsish | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish $ f a

newtype Mu f = InF { outF :: f (Mu f)}

instance Functor (f Mu)   where
   fmap f (InF f (Mu f)) = undefined
