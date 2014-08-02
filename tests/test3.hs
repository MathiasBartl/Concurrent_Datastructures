
-- puts pairs of keys and values into the table such that key==value and then checks if that is an invariant
module Main where



--import Test.HUnit
import qualified Data.LockFreeWaitFreeHashTable  as HT

import Control.Concurrent as C
import Control.Concurrent.Async as A
import Control.Monad (forM_)
import Data.Hashable

import Test.QuickCheck

hint = 2 ^ 13
range = [0..(2^12)]
repetition = 2^36
numberOfThreads = 32


data Inout = Put | Get

type HTActionParam = Int

type HTAction = (HTActionParam, Inout)

type ThreadParam = [HTAction]



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

testThread :: (Eq keyval, Hashable keyval) => HT.ConcurrentHashTable keyval keyval -> [(keyval, Inout)] -> IO ()
testThread ht param = withAsync (test ht param) undefined 

h2 :: HT.ConcurrentHashTable HTActionParam HTActionParam -> [ThreadParam] -> IO ()
h2 ht paramlist = forM_ paramlist (testThread ht)


--TODO: testcase generator witch is in IO any case, best probably Quickcheck


--TODO main method
main :: IO ()
main = return ()
