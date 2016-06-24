module Exercises where

import Control.Applicative ((*>))
import Control.Monad (Monad)

main :: IO()
main = return ()

join :: Monad m => m (m a ) -> m a
join = undefined

bind :: Monad m =>  (a -> m b) -> m a -> m b
bind f  = join . fmap  f


sequencing :: IO()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"


sequencing' :: IO()
sequencing' =
  putStrLn "blah" >> putStrLn "another thing"

sequencing'' :: IO()
sequencing'' = putStrLn "blah" *> putStrLn "another thing"

data Sum  a b =
  First a |
  Second b
  deriving (Eq, Show)

instance Functor (Sum a ) where
  fmap _ (First x)  =  First x
  fmap f (Second x) = Second (f x)


instance Applicative (Sum a) where
  pure x = Second x
  _ <*> (First e)  = First e
  (First e) <*> _  = First e
  (Second f) <*> (Second x) = Second $ f x

instance Monad (Sum a) where
   return = pure
   (First a) >>= _ = First a
   (Second x ) >>= f = f x
