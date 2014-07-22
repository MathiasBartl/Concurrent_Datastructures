

module ConncurentHashmapTests where

import Test.HUnit
import qualified Placeholder as HT

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
 		      assertBool "should fail" False
	)
