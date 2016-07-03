module Lib
    ( someFunc
    ) where

import Data.Monoid ((<>))
import Data.Text
import Development.HgRev.TH (defFormat, hgRevStateTH, jsonFormat)
import Options.Applicative  (Parser, ParserInfo, execParser, fullDesc,
                                        help, helper, info, infoOption, long,
                                        progDesc, short)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

jsonSwitch :: Parser (a -> a)
jsonSwitch =
  infoOption $ (hgRevStateTH jsonFormat)
  $ long "json"
  <> short 'J'
  <> help "Display JSON version information"
