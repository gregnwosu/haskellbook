module Lib (someFunc) where

import Control.Monad
import Data.Functor.Identity
import Data.Functor.Constant
import Data.Monoid
import Prelude hiding (Either, Left , Right)
import Test.QuickCheck
import Test.QuickCheck.Checkers

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

someFunc :: IO()
someFunc = putStrLn "someFunc"

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn =  fetchFn >=>  traverse makeIoOnlyObj . traverse decodeFn

edgelordMap :: Traversable t => (a -> b) -> t a -> t b
edgelordMap f t = runIdentity $ traverse (Identity . f) t

xs = [1,2,3,4,5] :: [Sum Integer]

foldMap' :: (Traversable t, Monoid b) => (a -> b) -> t a -> b
foldMap' f t = getConstant $ traverse (Constant . f) t

data Either a b =
  Left a |
  Right b  deriving (Eq, Ord, Show)

instance Functor (Either a ) where
  fmap _ (Left x)  = Left x
  fmap f (Right y) = Right (f y)

instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = Left e
  Right f <*> r = fmap f r

instance Foldable (Either a) where
  foldMap _ (Left _) = mempty
  foldMap f (Right y) = f y

instance Traversable (Either a) where
 traverse _ (Left x)  = pure (Left x)
 traverse f (Right y) = pure <$> f y

newtype Tuple a b = Tuple (a,b)

instance Functor (Tuple a) where
   fmap f (Tuple (a,b)) = Tuple (a, f b)

instance Monoid a => Applicative (Tuple a) where
  pure x = Tuple (mempty, x)
  (Tuple (a, f)) <*> (Tuple (a', x)) = Tuple (a <> a', f x)

instance Foldable (Tuple a) where
  foldMap f (Tuple (_, y)) = f y
  foldr f z (Tuple (_, y)) = f y z

instance Traversable (Tuple a) where
  traverse f (Tuple (a,b)) = (\x -> Tuple (a, x)) <$> f b

