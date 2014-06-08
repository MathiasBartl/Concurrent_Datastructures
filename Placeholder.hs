
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
    Concurrent Hashmap - 
    Copyright (C) 2014  Mathias Bartl

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-} 



module Data.HashTables.IO.Placeholder 
	( ConcurrentHashTable, size, isEmpty, containsKey, containsValue, put, putIfAbsent, removeKey, remove, replace , replaceTest, clear, get 
	)
	where

import GHC.IORef(IORef(IORef), readIORef, newIORef, writeIORef)
import Data.Hashable(Hashable, hash)
import Data.Bits((.&.), shiftR) 
import Data.Atomics
--todo restrict and qualify
import Control.Exception(assert)
import Data.Atomics.Counter
import qualified Data.Vector as V



min_size_log = 3
min_size = 2 ^ min_size_log --must be power of 2, compiler should turn this into a constant


getMask:: Size -> Mask
getMask size = size -1 

--data representation
---------------------------------------------------------------------------------------------------------------------------------
-- Kempty : empty, K : neverchanging key
data Key key = Kempty | K key deriving (Eq) --TODO make instance of Eq 
-- T : empty, tombstone, Tp : tombstone primed, V : value, Vp : value primed
data Value value =  T | Tp |V value | Vp value deriving (Eq) --TODO what kind of comparision is used

data State k v =   State {
				key :: IORef (Key k)
				, value :: IORef (Value v)
				}


data Kvs k v =   Kvs {
	newkvs :: Maybe (IORef ( Kvs k v))
	,slots :: Slots k v 
	, mask :: Mask
	, slotsCounter :: SlotsCounter
	, sizeCounter :: SizeCounter 
}


type SlotsCounter = AtomicCounter
type SizeCounter  = AtomicCounter

type FullHash = SlotsIndex
type SlotsIndex = Int
type Mask = SlotsIndex
type Size = Int
type SizeLog = Int

data ValComparator = MATCH_ANY | NO_MATCH_OLD

type ValComp val = Either (Value val) ValComparator 


type Slots key val = V.Vector (State key val)
--TODO issue accessing the array generates a full copy, fix this latter 

data ConcurrentHashTable key val = ConcurrentHashTable {	
		  kvs :: IORef(Kvs key val)
}
--------------------------------------------------------------------------------------------------------------------------------------------

-- does not terminate if array is full, and key is not in it
-- TODO, use fitting hash function
getSlot :: forall key value . (Hashable key, Eq key) =>  
           Slots key value -> Mask -> key -> IO(State key value)
getSlot slots mask key =  do	let idx = hsh key mask
                                    newkey = K key 
				slot <- getSlt slots newkey idx mask
--collision treatment has to be done again on a write should the key cas fail
				return slot
		where hsh :: (Hashable key) => key -> Mask -> SlotsIndex
		      hsh k m = (hash k)  .&. m
		      collision :: SlotsIndex -> Mask -> SlotsIndex
		      collision idx mask = (idx +1) .&. mask
		      full :: Key key -> Key key -> Bool
		      full  Kempty _ = False
		      full  k1 k2 = not (keyComp k1 k2)
		      getSlt:: Slots key value -> Key key -> SlotsIndex -> Mask -> IO(State key value)
		      getSlt slots newkey idx mask =
                        do let slot = (slots V.! idx) :: (State key value)
                           oldkey <- (readKeySlot slot)::IO(Key key)
                           slot <- (if full oldkey newkey
                                    then getSlt slots newkey (collision idx mask) mask
                                    else return slot) :: IO (State key value)
                           return slot --TODO count reprobes 


unwrapValue :: Value val -> Maybe val
unwrapValue T = Nothing
unwrapValue Tp = Nothing
unwrapValue (V a) = Just a
unwrapValue (Vp a) = Just a

keyComp:: Eq key => 
          Key key -> Key key -> Bool
keyComp Kempty Kempty = True
keyComp Kempty _      = False
keyComp _     Kempty  = False
keyComp (K k1) (K k2) = k1 == k2 --TODO eqality on key what about hashes


isPrimedValue :: Value val -> Bool
isPrimedValue Tp = True
isPrimedValue (Vp _) = True
isPrimedValue _ = False

isPrimedValComp :: ValComp val -> Bool
isPrimedValComp (Left v) = isPrimedValue v
isPrimedValComp (Right _) = False

{--
lookup :: Hashable key => ConcurrentHashTable key val -> key -> IO ( Maybe val)
lookup table k = do
		slot <- getSlot table k
		slotkey <- readIORef ( key slot)
		slotvalue <- readIORef (value slot)
		return $ getValue slotkey slotvalue
	where --getSlot :: ConcurrentHashTable key val -> key -> State key val
	      --getSlot tbl key = getSlot (slots tbl) (mask tbl)  
 
	      getValue :: Key key -> Value val -> Maybe val
	      getValue Kempty _ = Nothing
	      getValue (K _) v  = Just v 
--}



--see get
get_impl :: (Eq key, Hashable key) => 
            ConcurrentHashTable key val -> Kvs key val -> key -> FullHash -> IO(Value val)
get_impl table kvs key fullhash = do let msk = mask kvs
                                         slts = slots kvs
				     slt <- getSlot  slts msk key --TODO pass fullhash
				     k <- readKeySlot slt
				     v <- readValueSlot slt
				     if keyComp k ( K key) then return v 
                                     	else return T  --TODO use hash-caching for keycompare
--TODO actually we could use IO(Maybe (Value val)) as return type
			
--TODO treat resize

--TODO only pass reference to table if necessary
--TODO fit get function with table resizing

--Accessing the slot
--------------------------------------------------------------------------------------------------------------------------------

readKeySlot:: State key value -> IO (Key key)
readKeySlot state = readIORef ( key state  )
		
readValueSlot:: State key value -> IO (Value value)
readValueSlot state = readIORef ( value state  )
	


--TODO Question When are 2 Keys equal, and why does ticket not require a to be in class eq, how exactly does the COMPARE part work
casKeySlot :: forall key value. (State key value) -> Key key -> Key key -> IO ( Bool )
casKeySlot (State ke va) old new = do
				oldref <- (newIORef old)::IO(IORef (Key key))
				newref <- (newIORef new)::IO(IORef (Key key))
				oldticket <- (readForCAS oldref) ::IO(Ticket(Key key))
				newticket <- (readForCAS newref) ::IO(Ticket(Key key))
				(returnvalue, _) <- (casIORef2 ke oldticket newticket)::IO(Bool, Ticket(Key key)) 
				return returnvalue					

--TODO, see casKeySlot
casValueSlot :: forall key value.
	        (State key value) -> Value value -> Value value -> IO ( Bool )
casValueSlot (State ke va) old new = do
				oldref <- (newIORef old)::IO(IORef (Value value))
				newref <- (newIORef new)::IO(IORef (Value value))
				oldticket <- (readForCAS oldref) ::IO(Ticket(Value value))
				newticket <- (readForCAS newref) ::IO(Ticket(Value value))
				(returnvalue, _) <- (casIORef2 va oldticket newticket)::IO(Bool, Ticket(Value value)) 
				return returnvalue

--setValueSlot :: forall key value. (State key value) -> Value value -> Value value -> IO ( Bool )




-------------------------------------------------------------------------------------------------------------------------

putIfMatch_T ::(Hashable key, Eq key, Eq value) =>
               ConcurrentHashTable key value -> key -> Value value -> ValComp value -> IO ()
putIfMatch_T table key putVal expVal = do let kvsref = kvs table
                                              ky = K key
                                          kv <- readIORef kvsref
                                          putIfMatch kv ky putVal expVal                                   
--TODO return oldvalue


--TODO, do we need to pass the Hashtable as parameter?
--TODO assert key is not empty, putval is no empty, but possibly a tombstone, key value are not primed 
putIfMatch :: forall key value. (Hashable key, Eq key, Eq value) =>
              Kvs key value -> Key key -> Value value -> ValComp value -> IO ()
putIfMatch kvs key putVal expVal = do
  let msk = mask kvs
      slts = slots kvs
  reprobe_cnt <- return 0
  return $ assert $ not $ keyComp key  Kempty --TODO use eq TODO this is not in the original
  return $ assert $ not $ isPrimedValue putVal
  return $ assert $ not $ isPrimedValComp expVal
	 
  K k   <- (return key)           ::IO(Key key)  --TODO pune this better
  slot  <- (getSlot slts msk k) ::IO(State key value)
  --TODO if putvall TMBSTONE and oldkey == empty do nothing
  oldKey <-  readKeySlot slot
  if oldKey == Kempty
    then (if putVal == T
          then return() {-TODO break writing value unnecessary -}
          else (do b <- undefined -- casKeySlot slot oldKey key
                   if b
                   then return (){- TODO write value, increase slot counter -}
                   else return (){- TODO reprobe-}) )
    else return () {- TODO test if it is the same key then either reprobe, or write value  -} 

--TODO when would cas fail
  return ()   
--TODO return oldvalue


-- counter functions
------------------------------------------------------------------------------------------------------------------------
-- | Increments the counter of used slots in the array.
incSlotsCounter :: Kvs key value -> IO ()
incSlotsCounter kvs = do let counter = slotsCounter kvs
			 incrCounter_ 1 counter


incSizeCounter :: Kvs key value -> IO ()
incSizeCounter kvs = do let counter = sizeCounter kvs
			incrCounter_ 1 counter
--TODO possibly parameter table

decSizeCounter :: Kvs key value -> IO ()
decSizeCounter kvs = do let counter = sizeCounter kvs
			incrCounter_ (-1) counter

readSlotsCounter :: Kvs key value -> IO Int
readSlotsCounter kvs = do let counter = slotsCounter kvs
                          readCounter counter

readSizeCounter :: Kvs key value -> IO Int
readSizeCounter kvs = do let counter = sizeCounter kvs
                         readCounter counter

newSizeCounter :: IO(SizeCounter) 
newSizeCounter = newCounter 0
--TODO possibly parameter table
--Exported functions
-------------------------------------------------------------------------------------------------------------

-- | Returns the number of key-value mappings in this map
size :: ConcurrentHashTable key val -> IO(Size)
size table = do let kvsref= kvs table
		kvs <- readIORef kvsref
		readSizeCounter kvs 



isEmpty :: ConcurrentHashTable key val -> IO(Bool)
isEmpty table = do
		 s <- size table
		 return $ s == 0 


-- | Tests if the key in the table
containsKey :: (Eq key, Eq val, Hashable key) =>
	       ConcurrentHashTable key val -> key -> IO(Bool)
containsKey table key = do
			value <- get table key			
			return $ not $ value == Nothing



containsValue ::  ConcurrentHashTable key val -> val -> IO(Bool)
containsValue = undefined
--TODO low priority

put :: (Eq val,Eq key, Hashable key) => 
       ConcurrentHashTable key val -> key -> val -> IO()
put table key val = putIfMatch_T table key (V val) (Right NO_MATCH_OLD)
--TODO middle priority
--TODO return old value

putIfAbsent :: (Eq val,Eq key, Hashable key) => 
               ConcurrentHashTable key val -> key -> val -> IO()
--TODO middle priority
putIfAbsent table key val = putIfMatch_T table key (V val) (Left T) --TODO is tombstone correct, what if there is a primed vaue 
--TODO return old value

-- | Removes the key (and its corresponding value) from this map.
removeKey :: (Eq val, Eq key, Hashable key) =>
             ConcurrentHashTable key val -> key -> IO()
--TODO middle priority
removeKey table key  = putIfMatch_T table key T (Right NO_MATCH_OLD)
--TODO return removed value

-- | Removes key if matched.
remove :: (Eq val, Eq key, Hashable key) =>
          ConcurrentHashTable key val -> key -> val -> IO()
--TODO middle priority
remove table key val = putIfMatch_T table key T (Left (V val))
--TODO return boolean


-- | do a put if the key is already mapped to some value
replace :: (Eq val, Eq key, Hashable key) =>
	   ConcurrentHashTable key val -> key -> val -> IO()
--TODO middle priority
replace table key val = putIfMatch_T table key (V val) (Right MATCH_ANY)
--TODO return old value

-- | do a put if the key is already mapped to the old value
replaceTest :: (Eq val, Eq key, Hashable key) =>
               ConcurrentHashTable key val -> key -> val -> val -> IO()
--TODO middle priority
replaceTest table key newval oldval= putIfMatch_T table key (V newval) (Left (V oldval))
--TODO return boolean

-- | Removes all of the mappings from this map, number of slots to min_size
clear :: ConcurrentHashTable key val -> IO()
clear table = clearHint table min_size 

-- | Removes all of the mappings from this map, number of slots to next largest power of 2
clearHint :: ConcurrentHashTable key val -> Size -> IO()
clearHint table hint = do let size = normSize hint
                              kvsref = kvs table
		          szcntr <- newSizeCounter
                          kvs <- newKvs size szcntr
			  writeIORef kvsref kvs
--TODO rewrite in case type of ConcurrentHashTable changes


-- | Returns the value to which the specified key is mapped.
get :: (Eq key, Hashable key) => 
       ConcurrentHashTable key val -> key ->  IO( Maybe val)
get table key = do let fullhash = hash key --TODO use the right hashfunctio here
		   topkvs <- readIORef $ kvs table            
		   result <- get_impl table topkvs key fullhash
		   return $ unwrapValue result


-- | Create a new NonBlockingHashtable with default minimum size (currently set
--    to 8 K/V pairs)
newConcurrentHashTable :: IO(ConcurrentHashTable key val)
newConcurrentHashTable = newConcurrentHashTableHint min_size

-- |Create a new NonBlockingHashtable with initial room for the given number of
-- elements, thus avoiding internal resizing operations to reach an
-- appropriate size. Large numbers here when used with a small count of
-- elements will sacrifice space for a small amount of time gained. The
-- initial size will be rounded up internally to the next larger power of 2.
newConcurrentHashTableHint :: Size -> IO(ConcurrentHashTable key val)
newConcurrentHashTableHint hint = do let size = normSize hint
                                     szcntr <- newSizeCounter
				     kvs <- newKvs size szcntr
				     kvsref <- newIORef kvs

				     return $ ConcurrentHashTable kvsref                                    
--TODO throw error if size <0

-- | Returns the next larger potency of 2
normSize:: Size -> Size
normSize inputSize = 2 ^ (  max (sizeHelp inputSize 0) min_size_log)
	where
		sizeHelp :: Size -> SizeLog -> SizeLog
		sizeHelp s l = if s==0 then l else sizeHelp (shiftR s 1) (l+1) --TODO fold this

--size has to be power of 2
newKvs :: Size -> SizeCounter-> IO(Kvs key val)
newKvs size  counter = do let msk = getMask size
		          slts <- newSlots size
	                  sltcntr <- newSlotsCounter
	                  return $ Kvs Nothing slts msk sltcntr counter
	where
		newSlots :: Size -> IO( Slots key val)
		newSlots size = V.replicateM size newSlot --TODO
		newSlotsCounter :: IO(SlotsCounter)
		newSlotsCounter = newCounter 0
		newSlot	:: IO(State key val)
		newSlot = do keyref <- newIORef Kempty
			     valref <- newIORef T     --TODO optimize somewhere, somewhat 
			     return $ State keyref valref 
		




