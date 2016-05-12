module ChapterSemigroupExercises where

import Data.List.NonEmpty
import Control.Monad (liftM)
import Data.Semigroup
import Test.QuickCheck


monoidAssoc :: (Semigroup a, Eq a) => a -> a -> a -> Bool
monoidAssoc a b c = (a <> b)  <> c == (a <> (b <> c))


data Trivial  = Trivial deriving (Eq, Show)
data Or a b = Fst a | Snd b deriving (Eq, Show)


instance Semigroup (Or a b) where
  (Snd x) <> (Fst y) = Snd x
  (Fst x) <> (Snd y) = Snd y
  (Snd x) <> _ = Snd x
  _ <> (Fst x) = Fst x

newtype Identity a = Identity a
 deriving (Eq, Show)
newtype BoolConj  = BoolConj Bool deriving (Eq, Show)
newtype BoolDisj  = BoolDisj Bool deriving (Eq, Show)
newtype Combine a b = Combine {unCombine :: a -> b}
instance Semigroup BoolConj  where
  (BoolConj x) <> (BoolConj y) = BoolConj $ x && y

instance Semigroup BoolDisj  where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj $ x || y
instance Semigroup b => Semigroup (Combine a b) where
 (Combine f) <> (Combine g) =  Combine $  \a -> f a <> g a

newtype Three a b c= Three (a, b, c)
 deriving (Eq, Show)
newtype Four a b c d= Four (a,b,c,d)
 deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b , Arbitrary c , Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four (a,b,c,d)

instance (Arbitrary a, Arbitrary b , Arbitrary c ) => Arbitrary (Three a b c ) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three (a,b,c)

instance (Semigroup a , Semigroup b, Semigroup c ,Semigroup d) => Semigroup (Four a b c d ) where
  (Four ( a,b,c,d)) <> (Four (a',b',_,d')) = Four (a <> a', b <> b', c, d <>d')

instance Semigroup (Three a b c  ) where
  a <> _ = a

newtype Two a b = Two b
 deriving (Eq, Show)
instance (Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = liftM Two arbitrary
instance (Semigroup b) => Semigroup (Two a b) where
  (Two b) <> (Two b') = Two $ b <> b'
instance Semigroup Trivial where
  _ <> _ = Trivial

instance (Semigroup a) => Semigroup (Identity a) where
   (Identity a ) <> (Identity b) = Identity $ a <> b
instance Arbitrary Trivial where
  arbitrary = return Trivial
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = liftM Identity arbitrary

type Thr = Three String String Int
type Fore = Four String [Int] String [(Int,String)]
bools = forAll  (do
                     a<- arbitrary
                     b<- arbitrary
                     c<- arbitrary
                     return (a,b,c))
type Val = Validation (MySum Int, String) String

newtype Comp a = Comp { unComp :: a -> a }

newtype MySum a = MySum (Sum a) deriving (Eq, Show)
instance Arbitrary a => Arbitrary (MySum a) where
  arbitrary =  arbitrary >>= \a -> return $ MySum (Sum a)

instance (Num a) => Semigroup (MySum a) where
  (MySum a ) <> (MySum b) = MySum $ a <> b

instance Semigroup (Comp a) where
  f <> g = Comp $ unComp f .  unComp g

data Validation a b = Failure' a | Success' b
  deriving (Eq, Show)

newtype AccumulateRight a b =
  AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
    (AccumulateRight (Success' b))  <> (AccumulateRight (Success' b')) = AccumulateRight $ Success' $ b <> b'
    f <> (AccumulateRight (Success' b))  = f
    (AccumulateRight (Success' b)) <> f  = f
    _  <> d  = d

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary =
    do
      a <- arbitrary
      b <- arbitrary
      frequency [(1,return $ AccumulateRight (Failure' a)),
                 (1,return $ AccumulateRight (Success' b))]

type AccR = AccumulateRight String String
type AccB = AccumulateBoth String String

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
 (AccumulateBoth (Success' b)) <>  (AccumulateBoth (Success' b')) =
   AccumulateBoth (Success' $ b <> b')
 (AccumulateBoth (Failure' a)) <>  (AccumulateBoth (Failure' a')) =
   AccumulateBoth (Failure' $ a <> a')
 f <> (AccumulateBoth (Success' _)) = f
 (AccumulateBoth (Success' _)) <> f = f

instance (Arbitrary a , Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary =
    do
      a <- arbitrary
      b <- arbitrary
      frequency [(1, return $ AccumulateBoth (Success' a)),
                 (2, return $ AccumulateBoth (Failure' b))]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Failure' a, Success' b]

instance Semigroup a =>
  Semigroup (Validation a b) where
    (Success' b)  <> (Success' b') = Success' b'
    f <> (Success' b)  = f
    (Success' b) <> f  = f
    (Failure' a) <> (Failure' a') = Failure' $ a <> a'


main :: IO()
main = do
  quickCheck (monoidAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (monoidAssoc :: Identity String-> Identity String -> Identity String -> Bool)
  quickCheck (monoidAssoc :: Two () String-> Two () String -> Two ()String -> Bool)
  quickCheck (monoidAssoc :: Thr -> Thr -> Thr -> Bool)
  quickCheck (monoidAssoc :: Fore -> Fore -> Fore -> Bool)
  quickCheck (monoidAssoc :: Val -> Val -> Val -> Bool)
  quickCheck (monoidAssoc :: AccR -> AccR -> AccR -> Bool)
  quickCheck (monoidAssoc :: AccB -> AccB -> AccB -> Bool)
  quickCheck $ bools (\(a,b,c) -> monoidAssoc (BoolConj a) (BoolConj b) (BoolConj c))
  quickCheck $ bools (\(a,b,c) -> monoidAssoc (BoolDisj a) (BoolDisj b) (BoolDisj c))
