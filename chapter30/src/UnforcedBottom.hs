module UnforcedBottoms where

import Control.Exception

noWhammies :: IO (Either SomeException ())
noWhammies =
    try undefined

megaButtums :: IO (Either SomeException ())
megaButtums =
   try $ return undefined


-- The issue is that nonstrictness means burying the bottom in a return causes the bottom not to get forced until youre already past the try resulting in an uncaught error inside the Right constructor.
