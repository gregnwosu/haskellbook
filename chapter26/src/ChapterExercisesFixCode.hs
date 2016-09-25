module ChapterExercisesFixCode where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad


isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite  = do
  v <- lift getLine
  guard $ isValid v
  return v


doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  putStrLn $ maybe "MOAR EXCITE" ((++) "Good, was very excite: " ) excite
