module Functions1 where


waxOn :: Num a => a -> a
waxOn = (*3)


main :: IO ()
main = do
  let _ = waxOn
  return ()
