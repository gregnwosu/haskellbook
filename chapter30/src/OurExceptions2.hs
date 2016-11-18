module OurExceptions2 where

import Control.Exception

data EATD =
    NotEven Int |
    NotDivThree Int
    deriving (Eq, Show)

instance Exception EATD

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO (NotDivThree i)
  | even i = throwIO (NotEven i)
  | otherwise =  return i

-- now to catch a family of exception

type EA e = IO (Either e Int)
tryNotEven = try (evenAndThreeDiv 0) :: EA EATD
tryNotDivThree = try (evenAndThreeDiv 1) :: EA EATD
