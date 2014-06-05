
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

import GHC.IORef(IORef(IORef), readIORef, newIORef)
import Data.Array.Unboxed(UArray)
import Data.Hashable(Hashable, hash)
-- import Data.Array.IArray((!))
import Data.Array.Unboxed((!), IArray)
import Data.Bits((.&.)) 
import Data.Atomics
--todo restrict and qualify
import Control.Exception(assert)
import Data.Atomics.Counter



min_size_log = 3
min_size = 2 ^ min_size_log --must be power of 2, compiler should turn this into a constant


getMask:: Int -> Mask
getMask size = size -1 --if size_log == 1 then 1 else (getMask ( size_log -1) )* 2 + 1

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
	newkvs :: IORef ( Maybe (Kvs k v))
	,slots :: Slots k v 
	, mask :: Mask
	, slotsCounter :: SlotsCounter 
}


type SlotsCounter = AtomicCounter

type SlotsIndex = Int
type Mask = SlotsIndex
type Size = Int


type Slots key val = UArray SlotsIndex (State key val) --TODO issue accessing the array generates a full copy, fix this latter 

data ConcurrentHashTable key val = ConcurrentHashTable {
		--slots :: Slots key val --TODO, even if imutable it should be an IORef otherwise every handl to the hashmap will contain the whole array		
		  kvs :: IORef(Kvs key val)
		--,mask :: Mask --TODO make this IORef, in any case since resizing there should be multipe masks
}
--------------------------------------------------------------------------------------------------------------------------------------------

-- does not terminate if array is full, and key is not in it
-- TODO, use fitting hash function
--getSlot :: Hashable key => Slots key val -> Mask -> key -> State key val
--getSlot :: Hashable key => UArray SlotsIndex (State key val) -> Mask -> key -> State key val
--getSlot :: (Data.Array.Unboxed.IArray a e, Hashable key) =>  a SlotsIndex e -> Mask -> key -> e
--getSlot :: (Data.Array.Unboxed.IArray a Int, Hashable key) =>  a SlotsIndex Int -> Mask -> key -> Int
getSlot :: forall key value a. (Data.Array.Unboxed.IArray a (State key value), Hashable key, Eq key) =>  a SlotsIndex (State key value) -> Mask -> key -> IO(State key value)
getSlot slots mask key =  do	--slot <- (return ( slots ! ( hsh key mask)))::IO(State key value) 
				--oldkey <- (readKeySlot slot)::IO(Key key)
				idx <- return $ hsh key mask
				newkey <- (return $ K key)::IO(Key key)
				--slot <- (if full oldkey newkey then return slot else return slot)::IO(State key value) --TODO apply collision treatment
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
		      --getSlt:: Slots key value -> Key key -> SlotsIndex -> Mask -> IO(State key value)--TODO fix type
		      getSlt slots newkey idx mask = do
							slot <- (return (slots !  idx))::IO(State key value)
							oldkey <- (readKeySlot slot)::IO(Key key)
							slot <- (if full oldkey newkey then getSlt slots newkey (collision idx mask) mask else return slot)::IO(State key value)
							return slot --TODO count reprobes 

--new :: IO (ConcurrentHashTable a b)
--new = return $ ConcurrentHashTable $ newIORef $ Nothing array $ (0 , min_size -1) --TODO



keyComp:: Eq key => Key key -> Key key -> Bool
keyComp Kempty Kempty = True
keyComp Kempty _      = False
keyComp _     Kempty  = False
keyComp (K k1) (K k2) = k1 == k2 --TODO eqality on key waht about hashes


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


--gets the next index for collision treatment
--collision:: SlotsIndex -> Mask -> SlotsIndex

readKeySlot:: State key value -> IO (Key key)
readKeySlot state = do
			readIORef ( key state  )
		
readValueSlot:: State key value -> IO (Value value)
readValueSlot state = do
			readIORef ( value state  )
--TODO various functions that operate on Stat and use primeops
--TODO collision treatment	


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
casValueSlot :: forall key value. (State key value) -> Value value -> Value value -> IO ( Bool )
casValueSlot (State ke va) old new = do
				oldref <- (newIORef old)::IO(IORef (Value value))
				newref <- (newIORef new)::IO(IORef (Value value))
				oldticket <- (readForCAS oldref) ::IO(Ticket(Value value))
				newticket <- (readForCAS newref) ::IO(Ticket(Value value))
				(returnvalue, _) <- (casIORef2 va oldticket newticket)::IO(Bool, Ticket(Value value)) 
				return returnvalue

--setValueSlot :: forall key value. (State key value) -> Value value -> Value value -> IO ( Bool )


--TODO, do we need to pass the Hashtable as parameter?
--TODO assert key is not empty, putval is no empty, but possibly a tombstone, key value are not primed 
putIfMatch :: forall key value. (Hashable key, Eq key, Eq value) =>
              Kvs key value -> Key key -> Value value -> Value value -> IO ()
putIfMatch kvs key putVal expVal = do
  reprobe_cnt <- return 0
  return $ assert (keyComp key  Kempty) --TODO use eq
  slots <- (return $ slots kvs)   ::IO(Slots key value)
  mask  <- (return $ mask kvs)    ::IO(Mask)
  K k   <- (return key)           ::IO(Key key)
  slot  <- (getSlot slots mask k) ::IO(State key value) --FIXME Type problem
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



incSlotsCounter :: Kvs key value -> IO ()
incSlotsCounter kvs = do
			counter <- return $ slotsCounter kvs
			incrCounter_ 1 counter

--Exported functions
-------------------------------------------------------------------------------------------------------------

-- | Returns the number of key-value mappings in this map
size :: ConcurrentHashTable key val -> IO(Size)
--TODO low priority
size = undefined

isEmpty :: ConcurrentHashTable key val -> IO(Bool)
isEmpty table = return $ (size table) == 0 

-- | Tests if the key in the table
containsKey :: ConcurrentHashTable key val -> key -> IO(Bool)
containsKey table key = return $ not $ (get key) == Nothing



containsValue ::  ConcurrentHashTable key val -> val -> IO(Bool)
--TODO low priority
containsValue = undefined


put :: ConcurrentHashTable key val -> key -> val -> IO()
--TODO middle priority
put = undefined

putIfAbsent :: ConcurrentHashTable key val -> key -> val -> IO()
--TODO middle priority
putIfAbsent = undefined

-- | Removes the key (and its corresponding value) from this map.
removeKey :: ConcurrentHashTable key val -> key -> IO()
--TODO middle priority
removeKey = undefined

-- | Removes key if matched.
remove :: ConcurrentHashTable key val -> key -> val -> IO()
--TODO middle priority
remove = undefined

replace :: ConcurrentHashTable key val -> key -> val -> IO()
--TODO middle priority
replace = undefined

replaceTest :: ConcurrentHashTable key val -> key -> val -> IO(Bool)
--TODO middle priority
replaceTest = undefined

-- | Removes all of the mappings from this map.
clear :: ConcurrentHashTable key val -> IO()
--TODO low priority
clear = undefined

-- | Returns the value to which the specified key is mapped.
get :: ConcurrentHashTable key val -> key ->  IO( Maybe value)
--TODO High Priority
get = undefined

--TODO add new for default and arbitrary size 
--TODO somehow represent NO_MATCH_OLD and MATCH_ANY for putIfMatch 
