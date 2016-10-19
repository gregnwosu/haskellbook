{-# LANGUAGE BangPatterns #-}
--{-# LANGUAGE Strict #-}

module StrictList where

data List a =
  Nil |
  Cons a (List a)
  deriving Show

take' n _
      | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n-1) xs)

map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

repeat' x = xs where xs = Cons x xs

main = print $ take' 10 $ map' (+1) (repeat' 1)

-- 1. x = _
-- 2. x = _
-- 3. x = _
-- 4. x = 1
-- 5. x = _
-- 6. x = _
-- part 2
-- 1. no
-- 2. yes
-- 3. yes
-- 4. no
-- 5. no
-- 6. no
-- 7. yes
!x  = undefined
y = "blah"
!v = (x,y)
!main' = print $ snd v
