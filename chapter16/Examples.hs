module Examples where

class Sumthin a where
  s :: a -> a

class Else a where
  e :: b -> f (g a b c)


data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls  where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap :: Functor g => (b -> c) -> g b -> g c
-- (fmap.fmap) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
-- (.)  :: ((a -> b) -> f a -> f b) -> ((b -> c) -> g b -> g c) -> a -> c
-- (.) = f (g x)
-- (.) =

-- courtesy of http://adit.io/posts/2013-07-22-lenses-in-pictures.html
-- “If you want to do function composition where a function has two arguments”, says Sherlock, “you need (.).(.)!”
-- “That looks like a startled owl”, exclaims Watson.
-- “Indeed. Let’s see why this works.”
-- The type signature for function composition is:
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- Which looks a heck of a lot like fmap!
-- fmap :: (a -> b) -> f a -> f b
-- In fact if you replace a -> with f it’s exactly fmap!
-- And guess what! a -> is a functor! It’s defined like this:
-- instance Functor ((->) r) where
--    fmap = (.)
-- So for functions, fmap is just function composition! (.).(.) is the same as fmap . fmap!
-- (.).(.) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
-- fmap . fmap :: (a -> b) -> f (f1 a) -> f (f1 b)
-- There’s a pattern happening here: fmap . fmap and (.) . (.) both allow us to go “one level deeper”. In fmap it means going inside one more layer of functors. In function composition your functor is r ->, so it means you can pass in one more argument to your function.

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares  where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)
