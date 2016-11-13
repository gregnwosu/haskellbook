module Main where

import Criterion.Main
import Control.Monad.State.Lazy

data Queue a = Queue { enqueue :: [a], dequeue :: [a]}
             deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push x (Queue es ds) = Queue (x:es) ds

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue es []) = pop $ Queue [] (reverse es)
pop (Queue es (d:ds)) = Just (d, Queue es ds)

popT :: StateT (Queue a) Maybe a
popT = do
  q <- get
  (h,q') <- lift $ pop q
  put q'
  return h

pushT :: a -> StateT (Queue a) Maybe a
pushT x = do
  q <- get
  put (push x q)
  return x

pushpopT :: a ->   StateT (Queue a) Maybe a
pushpopT x  = do
  pushT x
  popT
--  pushT x

q = Queue [] []
iterateM :: Monad m => (a -> m a) -> m a-> Int -> m [a]
iterateM f mx n =
    sequence . take 1 . drop (n-1) . iterate (>>= f) $ mx
main :: IO ()
main =  defaultMain [

         bench "pushpop state monad queue implementation whnf" $
                            whnf (runStateT ( iterateM pushpopT (pushT 4 ) 50 )) q,
         bench "pushpop state monad queue implementation nf" $
                            nf (runStateT ( iterateM pushpopT (pushT 4 ) 50 )) q]
