
-- tests the concurrent hashtable for basic functionality in a single thread situation
module Main where

import Test.HUnit
import qualified Data.LockFreeWaitFreeHashTable  as HT

import Test.Framework (defaultMain, plusTestOptions)
import Test.Framework.Options ( TestOptions, TestOptions'(TestOptions), TestOptions')
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import Data.Monoid (mempty)

import Data.Bits(shiftL)
import Control.Monad (forM_, replicateM_, forM, replicateM)


setup :: IO ( HT.ConcurrentHashTable Int Int )
setup = do ht <- HT.newConcurrentHashTable
	   HT.put ht 10 10
	   HT.put ht 11 11
	   HT.put ht 12 12
           return ht 

emptySetup :: IO ( HT.ConcurrentHashTable Int Int )
emptySetup = HT.newConcurrentHashTable

isStrictOrdered xs = and $ zipWith (<) xs (tail xs)

--timelimit :: TestOptions
timelimit = TestOptions Nothing Nothing Nothing Nothing Nothing (Just (Just 60000))

tests = TestList [ TestLabel "test1" test1,
 TestLabel "put" test_put,
 TestLabel "put to erased slot"  test_put_to_erased_slot,
 TestLabel "get"  test_get,
 TestLabel "putIfAbsent" test_putIfAbsent,
 TestLabel "remove" test_remove,
 TestLabel "removeKey" test_removeKey,
 TestLabel "replace" test_replace,
 TestLabel "replaceTest" test_replaceTest,
 TestLabel "clear" test_clear,
 TestLabel "isEmpty" test_isEmpty,
 TestLabel "size" test_size,
 TestLabel "containsValue" test_containsValue,
 TestLabel "containsKey" test_containsKey,
 TestLabel "newConcurrentHashTableHint" test_newConcurrentHashTableHint,
 TestLabel "unnecessary_key_writes" test_unnecessary_key_writes]

tests_debugcode = TestList [ TestLabel "getNumberOfOngoingResizes" test_debugcode_getNumberOfOngoingResizes
			   , TestLabel "getLengthsOfVectors" test_debugcode_getLengthsOfVectors
			   , TestLabel "getSlotsCounters"  test_debugcode_getSlotsCounters]

tests_with_resize = TestList [ TestLabel "resize" test_resize
			     , TestLabel "multiple_resize" test_multiple_resize
			     , TestLabel "lotsof_put" test_lotsof_put
			--     , TestLabel ""
			     , TestLabel "resize_finishes_lotsof_get_tosame" test_resize_finishes_lotsof_get_tosame]

test1 = TestCase ( do ht <- (HT.newConcurrentHashTable)::IO(HT.ConcurrentHashTable Int Int) 
  		      ise <- HT.isEmpty ht
                      assertBool "Hashtable is empty" ise
  		      ret <-(HT.put ht 10 10)::IO(Maybe Int)
  		      assertBool  "No old value" (Nothing  == ret)
  		      sze <- HT.size ht
  		      assertBool  "Hashtable holds 1 Element" (1 == sze)
  		      ret <- (HT.get ht 10)::IO(Maybe Int)
  		      assertBool  "Basic put and get" (ret == Just 10)
  		      cnts <- HT.containsValue ht 10
  		      assertBool "Contains value" cnts
 		      cnts <- HT.containsValue ht 11
 		      assertBool "Doesn't contain value" (not cnts)
		      htstr <-  HT.debugShow ht
		      putStr htstr
 		
	)


--TODO some setup with put, and then seperate testcases for every function

test_put = TestCase (do ht <- setup
		        ret <- HT.put ht 10 16 --overwrite existing value
			assertEqual "should return old value" (Just 10) ret
			ret <- HT.put ht 10 17 --overwrite existing value again
			assertEqual "should return old value" (Just 16) ret
			ret <- HT.get ht 10
			assertEqual "new value" (Just 17) ret

		     	)

test_get = TestCase (do ht <- setup
			ret <- HT.get ht 10
			assertEqual "some value" (Just 10) ret
			ret <- HT.get ht 15
			assertEqual "empty slot" Nothing ret
                   )

--todo write to erased slot
test_putIfAbsent = TestCase ( do ht <- setup
				 ret <- HT.putIfAbsent ht 10 15  --write to occupied key, should not work
				 assertEqual "should return old value" (Just 10) ret
				 ret <- HT.putIfAbsent ht 16 16 --write to empty slot
				 assertEqual "should return Nothing" Nothing ret
				 ret <- HT.get ht 10
				 assertEqual "Value should be unchanged" (Just 10) ret
				 ret <- HT.get ht 16
				 assertEqual "Value should be unchanged" (Just 16) ret
				 
	)

test_remove = TestCase ( do ht <- setup
                            ret <- HT.remove ht 10 10
			    assertBool "sucessfull remove" ret
			    ret <- HT.remove ht 10 11
			    assertBool "unsucessfull remove" (not ret)
			    ret <- HT.remove ht 15 15
			    assertBool "unsucessfull remove" (not ret)
			    ret <- HT.get ht 10
			    assertEqual "should return Nothing" Nothing ret
			    ret <- HT.get ht 11
			    assertEqual "Value should be unchanged" (Just 11) ret
			    )
			    --ret <- HT.getSlotsCounters ht
			    --assertEqual "Slotscounter" 3 (head ret))


test_removeKey = TestCase ( do ht <- setup
			       ret <- HT.removeKey ht 10
			       assertEqual "should return old value" (Just 10) ret
			       ret <- HT.removeKey ht 15
			       assertEqual "no old value" Nothing ret
			       ret <- HT.get ht 10
			       assertEqual "Value should be removed" Nothing ret
			       ret <- HT.get ht 11
			       assertEqual "Value should be unchanged" (Just 11) ret
				)


test_replace = TestCase ( do ht <- setup
			     HT.removeKey ht 12 --creates slot with key and tombstone
			     ret <- HT.replace ht 10 15
			     assertEqual "should return old value" (Just 10) ret
			     ret <- HT.replace ht 13 16
			     assertEqual "should return old nothing" Nothing ret
			     ret <- HT.replace ht 12 17
			     assertEqual "should return old nothing" Nothing ret
			     ret <- HT.get ht 10
			     assertEqual "Value should be changed" (Just 15) ret
			     ret <- HT.get ht 13
			     assertEqual "Value should be unchanged nothing" Nothing ret
			     ret <- HT.get ht 12
			     assertEqual "Value should be unchanged nothing" Nothing ret
			)


test_replaceTest = TestCase ( do ht <- setup
				 ret <- HT.replaceTest ht 10 15 10
				 assertBool "should work" ret
				 ret <- HT.get ht 10
				 assertEqual "new value" (Just 15) ret
				 ret <- HT.replaceTest ht 11 16 17
				 assertBool "should not work" (not ret)
				 ret <- HT.get ht 11
				 assertEqual "old value" (Just 11) ret
				 ret <- HT.replaceTest ht 20 21 20
				 assertBool "should not work" (not ret)
				 ret <- HT.get ht 20
				 assertEqual "no old value" Nothing ret )

test_clear = TestCase ( do ht <- setup
			   HT.clear ht
			   ret <- HT.isEmpty ht
			   assertBool "Empty ht" ret
			   ret <- HT.get ht 10
			   assertEqual "get from empty table" Nothing ret 
			   assertSlotsCountersMatchUsedSlots ht)


test_put_to_erased_slot = TestCase ( do ht <- setup
				        HT.removeKey ht 10
					ret <- HT.put ht 10 10
					assertEqual "put into empty slot" Nothing ret
					ret <- HT.get ht 10
					assertEqual "new value" (Just 10) ret
				        assertSlotsCountersMatchUsedSlots ht
			                )

test_isEmpty = TestCase ( do ht <- setup
			     ret <- HT.isEmpty ht
			     assertBool "not empty" (not ret)
			     HT.put ht 10 11
			     HT.removeKey ht 12
			     ret <- HT.isEmpty ht
			     assertBool "not empty" (not ret)
			     HT.removeKey ht 11
	     		     ret <- HT.isEmpty ht
			     assertBool "not empty" (not ret)
  			     HT.removeKey ht 10
	     		     ret <- HT.isEmpty ht
			     assertBool "empty" ret
			     HT.put ht 20 20
			     ret <- HT.isEmpty ht
			     assertBool "not empty" (not ret) )


test_size = TestCase (do ht <- setup
			 ret <- HT.size ht
			 assertEqual "size 3" 3 ret
			 HT.put ht 10 10
			 ret <- HT.size ht
			 assertEqual "size 3" 3 ret
			 HT.remove ht 10 10
			 ret <- HT.size ht
			 assertEqual "size 2" 2 ret
			 HT.removeKey ht 11
			 HT.removeKey ht 12
			 ret <- HT.size ht
			 assertEqual "size 0" 0 ret
			 HT.removeKey ht 20 --not in ht
			 ret <- HT.size ht
			 assertEqual "size 0" 0 ret
			 HT.put ht 10 10
			 ret <- HT.size ht
			 assertEqual "size 1" 1 ret
			 ) 


test_containsValue = TestCase ( do ht <- setup 
				   ret <- HT.containsValue ht 20
				   assertBool "value isn't in ht" (not ret)
				   ret <- HT.containsValue ht 10
				   assertBool "value is in ht" ret )



test_containsKey =  TestCase (do ht <- setup 
				 ret <- HT.containsKey ht 20
				 assertBool "key isn't in ht" (not ret)
				 ret <- HT.containsKey ht 10
				 assertBool "key is in ht" ret 
				 --key with a tombstone should not be counted)
				 HT.removeKey ht 10
			         ret <- HT.containsKey ht 10
				 assertBool "key is deleted" (not ret) )

test_newConcurrentHashTableHint = TestCase (do ht <- (HT.newConcurrentHashTableHint 35)::IO(HT.ConcurrentHashTable Int Int)
					       lstret <- HT.getLengthsOfVectors ht	
					       assertEqual "Next highest potency of 2" 64 (head lstret)
					       ht <- (HT.newConcurrentHashTableHint 128)::IO(HT.ConcurrentHashTable Int Int)
					       lstret <- HT.getLengthsOfVectors ht	
					       assertEqual "exact potency of 2" 128 (head lstret)
					       --tests for Hints smaller than the minimum table size
					       ht <- (HT.newConcurrentHashTableHint 2)::IO(HT.ConcurrentHashTable Int Int)
					       lstret <- HT.getLengthsOfVectors ht	
					       assertEqual "minimum size" 8 (head lstret))
					       --tests for large Hints
					       --ATTENTION takes GBs of RAM
					       --ht <- (HT.newConcurrentHashTableHint (maxBound::Int))::IO(HT.ConcurrentHashTable Int Int)
					       --lstret <- HT.getLengthsOfVectors ht	
					       --assertEqual "minimum size" ( shiftL 1 31) (head lstret))
					       --test failed to complete after allocating 16 GB of ram
					       --TODO make a high ram usage flag	
					      
				 


test_unnecessary_key_writes = TestCase (do ht <- setup
					   ret <- HT.getSlotsCounters ht
					   assertEqual "SlotsCounter after 3 Inserts" 3 (head ret)
	
					   HT.removeKey ht 10
					   ret <- HT.getSlotsCounters ht  --TODO factor this out into an seperate Testcase
					   assertEqual "removal does not reduce the number of Slots in use" 3 (head ret)

				       	   HT.removeKey ht 20
					   ret <- HT.getSlotsCounters ht
					   assertEqual "Using up the slot should be unnecessary" 3 (head ret)

					   HT.remove ht 21 21
					   ret <- HT.getSlotsCounters ht
					   assertEqual "Using up the slot should be unnecessary" 3 (head ret)
					   --TODO test al other functions, witch may find an Tombstone and leave it there

					   HT.replace ht 22 22
					   ret <- HT.getSlotsCounters ht
					   assertEqual "Using up the slot should be unnecessary" 3 (head ret)
					   --replaceTest
					   HT.replaceTest ht 23 24 23
					   ret <- HT.getSlotsCounters ht
					   assertEqual "Using up the slot should be unnecessary" 3 (head ret)

					   assertSlotsCountersMatchUsedSlots ht
					   )

test_debugcode_getNumberOfOngoingResizes = TestCase (do ht <- (HT.newConcurrentHashTable)::IO(HT.ConcurrentHashTable Int Int)
							intret <- HT.getNumberOfOngoingResizes ht
							assertEqual "Resize not yet implemented" 0 intret)


test_debugcode_getLengthsOfVectors = TestCase (do ht <- setup
					          lstret <- HT.getLengthsOfVectors ht	
						  assertEqual "Resize not yet implemented" 1 (length lstret)
						  assertEqual "InitialDefault" 8 (head lstret))


test_debugcode_getSlotsCounters = TestCase (do ht <- setup
					       ret <- HT.getSlotsCounters ht
					       assertEqual "SlotsCounter after 3 Inserts" 3 (head ret)	
					       HT.removeKey ht 10 
					       ret <- HT.getSlotsCounters ht  --TODO factor this out into an seperate Testcase
					       assertEqual "removal does not reduce the number of Slots in use" 3 (head ret)
				       	       assertSlotsCountersMatchUsedSlots ht

					       --TODO compare slotscouter to actuall numberof used slots, possibly by writing an assertion into putIfMatch under the condition that all access is sequential
					       )

test_resize = TestCase (do ht <- emptySetup
			   forM_ [0..11] (\i -> HT.put ht i i)

			   ret <- HT.size ht
			   assertEqual "sizeCounter after resize" 12 ret
			

			   ret <- HT.getSlotsCounters ht
			   assertBool "ht has resized" ((last ret) > 8)
			   )


test_multiple_resize = TestCase (do ht <- emptySetup
				    forM_ [0..35000] (\i -> HT.put ht i i)
				    ret <- HT.size ht
				    assertEqual "sizeCounter after resize" 30000001 ret
				    ret <- HT.getSlotsCounters ht
				    assertBool "ht has resized" ((last ret) > 8)
				    assertBool "growns with every resize" (isStrictOrdered ret)
				    )


test_lotsof_put = TestCase ( do ht <- emptySetup
				forM_ [0..30000000] (\i -> HT.put ht i i)

				ret <- HT.size ht
				assertEqual "sizeCounter after resize" 30000001 ret
				
				ret <- forM [0..30000000] (\i -> HT.get ht i)

				assertEqual "No put left behind on resize" ((map Just) [0..30000000]) ret --TODO does this work
				)




test_resize_finishes_lotsof_get_tosame = TestCase (do ht <- emptySetup
				 		      forM_ [0..200] (\i -> HT.put ht i i)

				 		      ret <- HT.size ht
				 		      assertEqual "sizeCounter after resize" 201 ret
						      replicateM_ 8000 (HT.get ht 10)

				 		      ret <- HT.getSlotsCounters ht
				 		      assertBool "ht has resized" ((last ret) > 8)
				 		      assertEqual "all resizes are finished" 1 (length ret)
						      )

--TODO testcase for sizecouter
--TODO testcased for other fuctions when apropriate

--TODO write assertion that resizing grows, or does it

--checks if the slots counters fits the number of used slots, may fail spuriously, if ht is used concurrently due ton non-atomicy of keywrite+slotscounter increase
assertSlotsCountersMatchUsedSlots :: HT.ConcurrentHashTable key val -> Assertion
assertSlotsCountersMatchUsedSlots ht = do counters <- HT.getSlotsCounters ht
					  actuallyUsed <- HT.countUsedSlots ht
					  assertEqual "Slotcounters match used slots" actuallyUsed counters

main :: IO ()
main = defaultMain ((map (plusTestOptions timelimit)(hUnitTestToTests tests)) ++ (map (plusTestOptions timelimit) (hUnitTestToTests tests_debugcode)) ++ (map (plusTestOptions timelimit) (hUnitTestToTests tests_with_resize))) --FIXME looks bad
