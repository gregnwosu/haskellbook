module Main where

import Criterion.Main
import Control.Monad.State.Lazy
import Control.DeepSeq
import qualified Data.Sequence as SQ

seqs :: [SQ.Seq Int]
seqs = replicate 10 (SQ.fromList [1..1000000])

data Queue a = Queue { enqueue :: [a], dequeue :: [a]}
             deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push x (Queue es ds) = Queue (x:es) ds

instance NFData a => NFData (Queue a) where
    rnf (Queue a b) = a  `deepseq` (b `deepseq` ())

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue es []) = pop $ Queue [] (reverse es)
pop (Queue es (d:ds)) = Just (d, Queue es ds)

pushPop :: a -> Queue a -> Queue a
pushPop x q =
    let q' = push x q
        q'' = maybe q' snd (pop q') in q''

pushPopSQ ::  a -> SQ.Seq a -> SQ.Seq a
pushPopSQ x sq = let sq' = sq SQ.|> x
                     sq'' = SQ.drop 1 sq'
                 in sq''
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

ioTest = return (  runStateT ( iterateM pushpopT (pushT "help?!" ) 50 ) q)
main :: IO ()
main =  defaultMain [
         bench "pushPop q nf" $ nf (take 100 . iterate (pushPop "test") ) q,
         bench "pushPop seq nf" $ nf (take 100 . iterate (pushPopSQ "test") ) SQ.empty,
         bench "pushpop state monad queue implementation whnf" $
                            whnf (runStateT ( iterateM pushpopT (pushT 4 ) 50 )) q,
         bench "pushpop state monad queue implementation nf" $
                            nf (runStateT ( iterateM pushpopT (pushT "nf test" ) 50 )) q,
         bench "pushpop state monad queue implementation whnfIO" $
                            whnfIO ioTest ]
