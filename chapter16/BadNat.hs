{-# LANGUAGE RankNTypes #-}
module BadNat where

type Nat f g a = f a -> g a
type Nat' f g = forall a. f a -> g a

-- because Nat' has variables of kind * -> *  in its type constructor we cannot constrain the functors type variable as its not visible in the type declaration
maybeToList' ::  Nat' Maybe  []
maybeToList' Nothing = []
maybeToList' (Just a) = [a]


maybeToList :: Nat Maybe [] a
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- this shouldnt work because we shouldnt be allowed to do anything with a
degenerateMtl :: Num a => Nat Maybe [] a
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a+1]
