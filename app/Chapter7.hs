module Chapter7 where

tensDigit :: Integral  a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10


tensDigit2 :: Integral  a => a -> a
tensDigit2 x = d
  where (q,_) = x `divMod` 10
        d = q `mod` 10

hunsD :: Integral t => t -> t
hunsD x = d
  where (q,_) = x `divMod` 100
        d = q `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool a b tf = case tf of
  True -> a
  _ -> b

foldBool2 :: a -> a -> Bool -> a
foldBool2 a b tf
  | tf = a
  | otherwise = b


g :: (a ->b) -> (a,c) -> (b,c)
g f (a,c) = (f a, c)

roundTrip :: (Show a , Read b) => a -> b
roundTrip a = read (show a)



roundTripPF  :: (Show a , Read b) => a -> b
roundTripPF = read . show

testRT = do
  print(roundTripPF 4 :: Int)