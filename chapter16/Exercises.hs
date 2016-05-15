module Exercises where

a = fmap (+1) $ read "[1]"::[Int]
b = (fmap.fmap) (++ "lol") (Just ["Hi", "Hello"])
c = fmap (*2) (\x -> x -2)
d = fmap ((return '1' ++ ) . show) (\x -> [x, 1..3])
e :: IO Integer
e =
  let
  ioi = readIO "1":: IO Integer
  changed = fmap (read . ("123" ++) .  show)  ioi
  in  fmap (*3) changed
