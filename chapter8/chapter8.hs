module Chapter7 where

mc91 :: Integral a => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ x+11
