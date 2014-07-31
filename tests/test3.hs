
-- puts pairs of keys and values into the table such that key==value and then checks if that is an invariant
module Main where



--import Test.HUnit
import qualified Data.Placeholder as HT

import Control.Monad (forM_)
import Data.Hashable

hint = 2 ^ 13
range = [0..(2^12)]
repetition = 2^36


data Inout = Put | Get

fits :: Eq keyval => Maybe keyval -> keyval -> Bool --TODO differet returntype
fits Nothing _ = True
fits (Just kv1) kv2 = kv1 == kv2

h1:: (Eq keyval, Hashable keyval) => HT.ConcurrentHashTable keyval keyval -> (keyval, Inout) -> IO ()--TODO throw something
h1 ht (kv, Put) = do ret <- HT.put ht kv kv
		     if fits ret kv then return () else undefined --TODO undefied is not pass
ht ht (kv, Get) = do ret <- HT.get ht kv 
		     if fits ret kv then return () else undefined --TODO undefied is not pass

test :: (Eq keyval, Hashable keyval) => HT.ConcurrentHashTable keyval keyval -> [(keyval, Inout)] -> IO ()
test ht caseList =  forM_ caseList (h1 ht) 



--TODO main method
main :: IO ()
main = undefined
