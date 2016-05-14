module ChapterSemigroupExercises where

import Data.List.NonEmpty
import Control.Monad (liftM)
import Data.Semigroup
import Test.QuickCheck


instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

newtype Mem s a = Mem {
  runMem :: s -> (a,s)
}

instance Monoid a => Monoid (Mem s a) where
  (Mem f) `mappend` (Mem f') = Mem $ \x -> let (a, s)   = f x
                                               (a', s') = f' s
                                           in (mappend a a',s')
  mempty = Mem $ \s -> (mempty,s)

f' :: (Num s ) => (Mem Int String)
f' = Mem $ \s -> ("hi", s+1)

semigroupAssoc :: (Semigroup a, Eq a) => a -> a -> a -> Bool
semigroupAssoc a b c = (a <> b)  <> c == (a <> (b <> c))

monoidLeftIdentity :: (Monoid a, Semigroup a, Eq a) => a -> Bool
monoidLeftIdentity a =  mempty <> a == a

monoidRightIdentity :: (Monoid a, Semigroup a , Eq a) => a -> Bool
monoidRightIdentity a = a <> mempty == a

data Trivial  = Trivial deriving (Eq, Show)
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance (Arbitrary a) => Arbitrary (Comp a) where
  arbitrary =  Comp <$> const <$> arbitrary


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
instance Monoid BoolConj where
  mappend = (<>)
  mempty = BoolConj True
instance Monoid BoolDisj where
  mappend = (<>)
  mempty = BoolDisj False

instance Semigroup BoolDisj  where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj $ x || y
instance Semigroup b => Semigroup (Combine a b) where
 (Combine f) <> (Combine g) =  Combine $  \a -> f a <> g a
instance (Semigroup b, Monoid b) =>  Monoid (Combine a b) where
  mappend  = (<>)
  mempty = Combine $ const mempty

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
instance (Semigroup b , Monoid b) => Monoid (Two a b) where
  (Two b) `mappend` (Two b')= Two (b <> b')
  mempty  = Two mempty
instance Semigroup Trivial where
  _ <> _ = Trivial

instance (Semigroup a) => Semigroup (Identity a) where
   (Identity a ) <> (Identity b) = Identity $ a <> b

instance (Semigroup a , Monoid a) => Monoid (Identity a) where
  mappend  = (<>)
  mempty = Identity (mempty)
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

instance Monoid (Comp a) where
  mappend = (<>)
  mempty = Comp id
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
  quickCheck (semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: Identity String-> Identity String -> Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String-> Bool)
  quickCheck (monoidLeftIdentity :: Identity String-> Bool)
  quickCheck (semigroupAssoc :: Two () String-> Two () String -> Two ()String -> Bool)
  quickCheck (monoidRightIdentity :: Two Int String-> Bool)
  quickCheck (monoidLeftIdentity :: Two Int String-> Bool)
  quickCheck (semigroupAssoc :: Thr -> Thr -> Thr -> Bool)
  quickCheck (semigroupAssoc :: Fore -> Fore -> Fore -> Bool)
  quickCheck (semigroupAssoc :: Val -> Val -> Val -> Bool)
  quickCheck (semigroupAssoc :: AccR -> AccR -> AccR -> Bool)
  quickCheck (semigroupAssoc :: AccB -> AccB -> AccB -> Bool)
  quickCheck $ bools (\(a,b,c) -> semigroupAssoc (BoolConj a) (BoolConj b) (BoolConj c))
  quickCheck $ bools (\(a,b,c) -> semigroupAssoc (BoolDisj a) (BoolDisj b) (BoolDisj c))
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  print $ runMem (f' `mappend` mempty) 0
  print $ runMem (mempty `mappend` f') 0
  print  (runMem mempty 0 :: (String, Int))
  print $ runMem (f' `mappend` mempty) 0 == runMem f' 0
  print $ runMem (mempty `mappend` f') 0 == runMem f' 0
