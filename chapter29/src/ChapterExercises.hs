module Main where

import Data.Char
import System.Environment
import System.IO
import qualified IniParser as P
import qualified Text.Trifecta as T
import System.Directory
import Data.List
import System.FilePath.Posix
import Text.Printf
import qualified Data.Map as M
baseChr :: Char -> Int
baseChr = subtract 65 . ord . toUpper

transform :: (Int -> Int -> Int) -> String -> String -> String
transform f ks (' ':ts) = ' ':transform f ks ts
transform f _ [] = []
transform f (k:ks) (t:ts) = let textnum = baseChr t
                                keynum = baseChr k
                                shiftAmount = textnum `f` keynum  `mod` 26
                            in chr ( 65 + shiftAmount ) : transform f ks ts

vigenère :: String -> String -> String
vigenère key   = transform (+) ( concat $ repeat key)

unvigenère :: String -> String -> String
unvigenère key   = transform (-) ( concat $ repeat key)

key = "ALLY"
forArg ("d": _) = unvigenère key
forArg ("e":_) = vigenère key

timeoutMillis :: [String] -> IO Int
timeoutMillis (_:t:_) = return $ read t
timeoutMillis  _ =
    hPutStr stderr "\ntimeout not supplied default to 5000 ms" >>
    return 5000

nextChar :: Char -> IO String
nextChar c = isEOF >>= go
             where go True = return []
                   go False = (c:) <$> readStdInTillEOF

readStdInTillEOF :: IO String
readStdInTillEOF = do
  c <- getChar
  nextChar c

main' :: IO ()
main' = do
         crypt <- forArg <$> getArgs
         timeout <-  getArgs >>= timeoutMillis
         isTimedOut <- hWaitForInput stdin timeout
         if isTimedOut
           then putStrLn " ">> readStdInTillEOF >>= print . crypt
           else hPutStr stderr "\ntimed out waiting for input"
parseConfig = T.parseString P.parseIni mempty

contentsTuple :: String -> IO (String, String)
contentsTuple filename  = do
         contents <- readFile filename
         return (filename, contents)
contentsTuples :: String -> IO [(String, String)]
contentsTuples  arg = do
                 path <- makeAbsolute arg
                 files' <- listDirectory path
                 let files = filter (isSuffixOf ".ini") files'
                 let fqfilenames = fmap (printf "%s%c%s" path pathSeparator) files
                 traverse  contentsTuple fqfilenames
stripFailures (_ ,(T.Failure _)) b = b
stripFailures (fn,(T.Success config)) b = ((fn,config):b)

main = do
  args <- getArgs
  fs <- concat <$> (traverse contentsTuples args)
  let lookupList = (\(f,s) -> (f, parseConfig s)) <$> fs
  let configList = foldr stripFailures [] lookupList
  let result = M.fromList configList
  mapM_ (putStrLn  . show) configList
  return $ result









    --
