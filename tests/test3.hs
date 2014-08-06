{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-} 

-- puts pairs of keys and values into the table such that key==value and then checks if that is an invariant
module Main where



--import Test.HUnit
import qualified Data.LockFreeWaitFreeHashTable  as HT

import Control.Concurrent as C
import Control.Concurrent.Async as A
import Control.Monad (forM_, forM, replicateM)
import Data.Hashable

import Test.QuickCheck
import Test.HUnit
import Test.Framework (defaultMain, plusTestOptions)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework.Options ( TestOptions, TestOptions'(TestOptions), TestOptions')

hint = 2 ^ 13
range = [0..(2^12)]
repetition = 2^36
numberOfThreads = 32

timelimit = TestOptions Nothing Nothing Nothing Nothing Nothing (Just (Just 60000))

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

test1 :: (Eq keyval, Hashable keyval) => HT.ConcurrentHashTable keyval keyval -> [(keyval, Inout)] -> IO ()
test1 ht caseList =  forM_ caseList (h1 ht) 

testThread :: (Eq keyval, Hashable keyval) => HT.ConcurrentHashTable keyval keyval -> [(keyval, Inout)] -> IO ()
testThread ht param = withAsync (test1 ht param) (\async -> return ()) 

h2 :: HT.ConcurrentHashTable HTActionParam HTActionParam -> [ThreadParam] -> IO ()
h2 ht paramlist = forM_ paramlist (testThread ht)


--TODO: testcase generator witch is in IO any case, best probably Quickcheck
--Instance of arbitrary for HTAction

--Testcase generator
-------------------------------------------------------------------------------------------------------------------------------------------
--instance Arbitrary HTActionParam where
--  arbitrary   = elements range

instance Arbitrary Inout where
  arbitrary = elements [Put, Get]

--instance Arbitrary HTAction where  --pair is defined in Test.QuickCheck.Arbitrary
  --arbitrary = do param <- elements range
	--	 command <- arbitrary
	--	 return $ (param, command)

instance Arbitrary ThreadParam where
  arbitrary = replicateM repetition arbitrary


--------------------------------------------------------------------------------------------------------------------------------------
--todo lots of puts

emptySetup :: IO ( HT.ConcurrentHashTable Int Int )
emptySetup = HT.newConcurrentHashTable

test_lotsof_put = TestCase ( do let numberOfThreads = 128
				    valuesPerThread = 10000000 
				ht <- emptySetup
				forM_ [0..(numberOfThreads-1)] 
					(\number -> withAsync (lotsof_put ht 
					[(0 + (valuesPerThread*number))..(valuesPerThread-1 + (valuesPerThread*number))])
					(\async -> do myblst <- wait async 
					              assertEqual "no puts forgotten during resize" 
							(map Just [(0 + (valuesPerThread*number))..(valuesPerThread-1 + (valuesPerThread*number))])
							myblst))
				)


lotsof_put :: ( HT.ConcurrentHashTable Int Int ) -> [Int] -> IO [Maybe Int]
lotsof_put ht inp = do forM_ inp (\int -> HT.put ht int int)
		       forM inp (\int -> HT.get ht int)



tests = TestList [ TestLabel "lotsof_put" test_lotsof_put]

--TODO main method
main :: IO ()
main = defaultMain (map (plusTestOptions timelimit) (hUnitTestToTests tests))
