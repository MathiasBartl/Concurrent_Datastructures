
-- tests the concurrent hashtable for basic functionality in a single thread situation
module Main where

import Test.HUnit
import qualified Data.Placeholder as HT

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

setup :: IO ( HT.ConcurrentHashTable Int Int )
setup = do ht <- HT.newConcurrentHashTable
	   HT.put ht 10 10
	   HT.put ht 11 11
	   HT.put ht 12 12
           return ht 

tests = TestList [ TestLabel "test1" test1,
 TestLabel "put" test_put,
 TestLabel "put to erased slot"  test_put_to_erased_slot,
 TestLabel "get"  test_get,
 TestLabel "putIfAbsent" test_putIfAbsent,
 TestLabel "reove" test_remove,
 TestLabel "removeKey" test_removeKey,
 TestLabel "replace" test_replace,
 TestLabel "replaceTest" test_replaceTest,
 TestLabel "clear" test_clear,
 TestLabel "isEmpty" test_isEmpty,
 TestLabel "size" test_size,
 TestLabel "cotainsValue" test_containsValue,
 TestLabel "containsKey" test_containsKey]

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
			    assertEqual "Value should be unchanged" (Just 11) ret)


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
			   assertEqual "get from empty table" Nothing ret )


test_put_to_erased_slot = TestCase ( do ht <- setup
				        HT.removeKey ht 10
					ret <- HT.put ht 10 10
					assertEqual "put into empty slot" Nothing ret
					ret <- HT.get ht 10
					assertEqual "new value" (Just 10) ret)

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
				 


--TODO testcase for sizecouter
--TODO testcased for other fuctions when apropriate


main :: IO ()
main = defaultMain (hUnitTestToTests test1)
