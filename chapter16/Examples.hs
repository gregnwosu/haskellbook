module Examples where

class Sumthin a where
  s :: a -> a

class Else a where
  e :: b -> f (g a b c)


data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls  where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

{-

 courtesy of http://stackoverflow.com/questions/23030638/how-fmap-fmap-typechecks
First let's change the type variables' names to be unique:

(.)  :: (a -> b) -> (r -> a) -> (r -> b)
fmap :: Functor f => (c -> d) -> f c -> f d
fmap :: Functor g => (x -> y) -> g x -> g y
Now the first parameter to . has type a -> b and we supply an argument of type (c -> d) -> (f c -> f d), so a is c -> d and b is f c -> f d. So so far we have:

(.) :: Functor f => -- Left operand
                    ((c -> d) -> (f c -> f d)) ->
                    -- Right operand
                    (r -> (c -> d)) ->
                    -- Result
                    (r -> (f c -> f d))
The second parameter to . has type r -> a a.k.a. r -> (c -> d) and the argument we give has type (x -> y) -> (g x -> g y), so r becomes x -> y, c becomes g x and d becomes g y. So now we have:

(.)       :: (Functor f, Functor g) => -- Left operand
                                       ((g x -> g y) -> (f (g x) -> f (g y))) ->
                                       -- Right operand
                                       ((x -> y) -> (g x -> g y)) ->
                                       -- Result
                                       (x -> y) -> f (g x) -> f (g y)
fmap.fmap :: (Functor f, Functor g) => (x -> y) -> f (g x) -> f (g y)

-}

{- (fmap.fmap) :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
 fmap :: Functor f => (i -> j) -> f i -> f j
 fmap :: Functor g => (x -> y) -> g x -> g y

 (.) :: (b -> c) -> (a -> b) -> a -> c
 substituting in fmaps
   b = (g x -> g y)
   c = (f g x -> f g y)
   a = x -> y
   i = g x
   j = g y
  -- substituting back into compose
. = ((g x -> g y)  -> (f (g x) -> f (g y)) ->
    ((x -> y) -> (g x -> g y)) ->
    (x -> y)  -> f(g x) -> f (g y)
-- applying the  first two arguments (fmap) gives

 fmap . fmap =     (x -> y)  -> f(g x) -> f (g y)



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
-}
data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares  where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)
