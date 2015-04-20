{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- puts pairs of keys and values into the table such that key==value and then checks if that is an invariant
module Main where



--import Test.HUnit
import qualified Data.HashTable.Concurrent  as HT

import Control.Concurrent as C
import Control.Concurrent.Async as A
import Control.Monad (forM_, forM, replicateM)
import Data.Hashable

import Test.QuickCheck as QC
import Test.QuickCheck.Monadic as QCM
import Test.Framework.Providers.API as PAPI
import Test.QuickCheck.Arbitrary as QCA
import Test.Framework.Providers.QuickCheck2 as  PQC
import Test.HUnit
import Test.Framework (defaultMain, plusTestOptions)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework.Options ( TestOptions, TestOptions'(TestOptions), TestOptions')


numberOfTests = TestOptions Nothing (Just 900) Nothing Nothing Nothing Nothing
hint = 2 ^ 13
range = [0..(2^12)]
repetition = 2^36
numberOfThreads = 32

emptySetup :: IO ( HT.ConcurrentHashTable HTActionParam HTActionParam )
emptySetup = HT.newConcurrentHashTable

emptySetupInt :: IO ( HT.ConcurrentHashTable Int Int )
emptySetupInt = HT.newConcurrentHashTable

timelimit = TestOptions Nothing Nothing Nothing Nothing Nothing (Just (Just 60000))

data Inout = Put | Get deriving Show

distribution = [(1,return Put),(3,return Get)]

newtype HTActionParam = HTActionParam Int deriving (Eq, Show)

--  instead of declaring newtyps and writing standart generators,
-- leafing them as default types and use forall with custom genrators would also be possible

instance Hashable HTActionParam where
	hashWithSalt s (HTActionParam i) = hashWithSalt s i
	hash (HTActionParam i) = hash i

newtype HTAction = HTAction (HTActionParam, Inout) deriving Show

newtype ThreadParam = ThreadParam [HTAction] deriving Show

newtype ThreadParams = ThreadParams [ThreadParam] deriving Show

fits :: Eq keyval => Maybe keyval -> keyval -> Bool --TODO differet returntype
fits Nothing _ = True
fits (Just kv1) kv2 = kv1 == kv2

h1:: HT.ConcurrentHashTable HTActionParam HTActionParam -> HTAction -> IO ()--TODO throw something
h1 ht (HTAction (kv, Put)) = do ret <- HT.put ht kv kv
		     		if fits ret kv then return () else undefined --TODO undefied is not pass
ht ht (HTAction (kv, Get)) = do ret <- HT.get ht kv 
		     		if fits ret kv then return () else undefined --TODO undefied is not pass

test1 :: HT.ConcurrentHashTable HTActionParam HTActionParam -> ThreadParam -> IO ()
test1 ht (ThreadParam caseList) =  forM_ caseList (h1 ht) 

testThread ::  HT.ConcurrentHashTable HTActionParam HTActionParam -> ThreadParam -> IO ()
testThread ht param = withAsync (test1 ht param) (\async -> return ()) 

h2 :: HT.ConcurrentHashTable HTActionParam HTActionParam -> ThreadParams -> IO ()
h2 ht (ThreadParams paramlist) = forM_ paramlist (testThread ht)


prop1 ::  ThreadParams -> QCM.PropertyM IO ()  
prop1 params = QCM.wp  emptySetup (\ht -> QCM.run $ h2 ht params )

prop2 :: ThreadParams -> QC.Property
prop2  params = QCM.monadicIO $ prop1 params  

--testable1 = undefined

--test2 :: (Test.QuickCheck.Testable
--             testable) => testable ->  PAPI.Test --TODO witch type
--test2 t = PQC.testProperty "test_consistency_without_resize" t
test2 :: PAPI.Test
test2 = plusTestOptions numberOfTests (PQC.testProperty "test_consistency_without_resize" prop2)
--TODO: testcase generator witch is in IO any case, best probably Quickcheck
--Instance of arbitrary for HTAction

--Testcase generator
-------------------------------------------------------------------------------------------------------------------------------------------
instance Arbitrary HTActionParam where
  arbitrary   = do i <- elements range
		   return $ HTActionParam i

instance Arbitrary Inout where
  arbitrary = frequency distribution

instance Arbitrary HTAction where  
  arbitrary = do param <- arbitrary
		 command <- arbitrary
		 return $ HTAction (param, command)

--todo limit ht action to rang

instance QCA.Arbitrary ThreadParam where
  arbitrary = do vec <- QCA.vector repetition
                 return $ ThreadParam vec

instance QCA.Arbitrary ThreadParams where
  arbitrary = do vec <- QCA.vector numberOfThreads
		 return $ ThreadParams vec

--------------------------------------------------------------------------------------------------------------------------------------




test_lotsof_put = TestCase ( do let numberOfThreads = 4
				    valuesPerThread = 1 
				ht <- emptySetupInt
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

--TODO test withou resize, no puts are forgotten

tests = TestList [ TestLabel "lotsof_put" test_lotsof_put]
--tests = TestList []  --FIXME get the resize relatet tests going with a time limit

--TODO main method
main :: IO ()
main = defaultMain $ (map (plusTestOptions timelimit) (hUnitTestToTests tests))  ++ [test2] 
