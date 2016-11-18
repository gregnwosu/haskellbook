module OurExceptions where

import Control.Exception

data NotDivThree =
     NotDivThree Int
     deriving (Eq, Show)

instance Exception NotDivThree

data NotEven =
     NotEven Int
     deriving (Eq, Show)

instance Exception NotEven

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
    | rem i 3 /= 0 = throwIO $ NotDivThree i
    | odd i = throwIO $ NotEven i
    | otherwise = return i


-- catching different defined exceptions

catchNotDivThree :: IO Int
                   -> (NotDivThree -> IO Int)
                   -> IO Int
catchNotDivThree = catch

catchNotEven :: IO Int
               -> (NotDivThree -> IO Int)
               -> IO Int
catchNotEven = catch


-- or with Try

type EA e = IO (Either e Int)

tryNotEven = try (evenAndThreeDiv 2) :: EA NotEven
tryNotDivThree = try (evenAndThreeDiv 2) :: EA NotDivThree

-- catching from a list we use catches

-- catches :: IO a -> [Handler a ] -> IO a
catchBoth :: IO Int -> IO Int
catchBoth ioInt =
    catches ioInt
     [
      Handler (\(NotEven _) -> return maxBound),
      Handler (\(NotDivThree _) -> return minBound)
     ]
