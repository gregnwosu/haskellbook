module Lib (someFunc) where

import System.Random
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.Monoid

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
   1  -> DieOne
   2  -> DieTwo
   3  -> DieThree
   4 -> DieFour
   5 -> DieFive
   6 -> DieSix

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1,6) s
      (d2, s2) = randomR (1,6) s1
      (d3, _) = randomR (1,6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n,s) <- randomR (1,6)
  return (intToDie n, s)

rollDy :: StateT StdGen Identity Int
rollDy = state $ randomR (1,6)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1,6))

rollDieThreeTimes' ::  State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

someFunc :: IO ()
someFunc = putStrLn "someFunc"

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

infinDie :: State StdGen [Int]
infinDie = sequence $ repeat rollDy

nDie :: Int -> State StdGen [Die]
nDie = flip replicateM rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty  = go 0 0
  where
   go :: Int -> Int -> StdGen -> Int
   go sum count gen
       | sum >= 20 = count
       | otherwise =
            let (die, nextGen) = randomR (1,6) gen
            in  go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN lim  = go 0 0
  where
   go :: Int -> Int -> StdGen -> Int
   go sum count gen
       | sum >= lim = count
       | otherwise =
            let (die, nextGen) = randomR (1,6) gen
            in  go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> ( Int, [Die])
rollsCountLogged lim   = ((,) <$> getSum . fst <*> snd) .  go 0 (mempty, mempty)
  where
   go :: Int -> (Sum Int, [Die]) -> StdGen -> (Sum Int, [Die])
   go sum (s,l) gen
       | sum >= lim = (s,l)
       | otherwise =
            let (die, nextGen) = randomR (1,6) gen
            in  go (sum + die) (s + 1,intToDie die:l) nextGen

rollsCountLogged' :: Int -> StdGen -> (Int, [Die])
rollsCountLogged' lim sgen = go 0 (evalState infinDie sgen)
            where
                  go s (x:xs)
                    | s >= lim = (0, [])
                    | otherwise =  (fst h + 1, intToDie x:snd h)
                          where h = go (s+x) xs


newtype Moi s a = Moi {runMoi :: s -> (a, s)}

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s -> let (a,s') = g s
                               in (f a, s')

instance Applicative (Moi s) where
  pure a  = Moi $ \s -> (a, s)
  (Moi f) <*> (Moi x) = Moi $ \s -> let (f', s') = f s
                                        (x', s'') = x s'
                                    in (f' x', s'')

instance Monad (Moi s) where
  return = pure
  (Moi x) >>= f =
    Moi $ \s -> let (a',s') = x s
                    (Moi sb) = f a'
                in  sb s'
