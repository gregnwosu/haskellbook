module Lib where

-- n.b. strictPattern (undefined, undefined) works
-- but strictPattern undefined fails
strictPattern :: (a,b) -> String
strictPattern (a,b) = const "Cousin It" a
lazyPattern :: (a,b) -> String
lazyPattern ~(a,b) = const "Cousin It" a
