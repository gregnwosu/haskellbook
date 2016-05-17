{-# LANGUAGE FlexibleInstances #-}
module ChapterExercises where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Arr

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

newtype Mu f = InF { outF :: f (Mu f )}

-- functor instance for Mu not possible


data D = D (Array Word Word) Int Int

data Sum a b = First b | Second a

instance Functor (Sum a) where
  fmap f (First a) = First $ f a
  fmap _  (Second b) = Second b


data Company a b c =
  DeepBlue a b | Something c

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c)= DeepBlue a c

data More b a =
  L a b a | R b a b
    deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f (Bloor b)= Bloor $ f b
  fmap _ (Desk a )= Desk a
  fmap _ Finance = Finance

data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K $ f b)


data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a =
  LiftItOut (f a)

instance (Functor x) => Functor (LiftItOut x) where
  fmap f (LiftItOut x) = LiftItOut $ fmap f x

data Parappa f g a =
  DaWrappa (f a ) (g a)

instance (Functor x, Functor y) => Functor (Parappa x y) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x g) = IgnoringSomething x (fmap f g)


data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious g o a) = Notorious g o $ fmap f a


data List a =
  Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)



data GoatLord a =
  NoGoat |
  OneGoat a |
  MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor (GoatLord) where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)
