
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-} --FIXME only needed for debug code
{-# LANGUAGE FlexibleInstances #-}    --dito


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

-- TODO make assertEqualOrNothing



--TODO Data.HashTables... or Data.Concurrent...
--something like Data.HashTables.IO.NonBlocking.something
--               Data.HashTables.IO.Concurrent.NonBlocking.something
--TODO make list of all Hashtable libraries in haskell and compare
module Data.Placeholder 
	( 
          -- * Creating hash tables
          ConcurrentHashTable
	, newConcurrentHashTableHint, newConcurrentHashTable
          -- * Predicates and properties 
        , size, isEmpty, containsKey, containsValue
          -- * Basic reading and writing 
        , put, putIfAbsent, get 
          -- * Removing or replacing 
        , removeKey, remove, replace, replaceTest, clear

	  -- * Debuging
	, debugShow, getNumberOfOngoingResizes, getLengthsOfVectors
        )
	where

import GHC.IORef(IORef(IORef), readIORef, newIORef, writeIORef)
import Data.Hashable(Hashable, hash)
import Data.Bits((.&.), shiftL, shiftR) 
import Data.Atomics
--todo restrict and qualify
import Control.Exception(assert)
import Data.Atomics.Counter
import qualified Data.Vector as V
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Either.Unwrap (fromLeft, isLeft, isRight, fromRight)

import Numeric (showIntAtBase) --FIXME for debug only
import Data.Char (intToDigit)       --dito



min_size_log = 3
min_size = 2 ^ min_size_log --must be power of 2, compiler should turn this into a constant

max_size_log = 31
max_size = 2 ^ 31

getMask:: Size -> Mask
getMask size = size -1 

--data representation
---------------------------------------------------------------------------------------------------------------------------------
-- Kempty : empty, K : neverchanging key
data Key key = Kempty | K key deriving (Eq) --TODO make instance of Eq --TODO do keys need to be primed
-- T : empty, tombstone, Tp : tombstone primed, V : value, Vp : value primed --TODO need tobstoes to be primed
data Value value =  T | Tp |V value | Vp value | S deriving (Eq) --TODO what kind of comparision is used



data Slot k v =   Slot {
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

type ReprobeCounter = Int

newReprobeCounter = 0

data ValComparator = MATCH_ANY | NO_MATCH_OLD deriving Eq

type ValComp val = Either (Value val) ValComparator 

type ReturnValue val = (Bool, Value val)    --TODO find good name for this type, write description

type Slots key val = V.Vector (Slot key val)
--TODO issue accessing the array generates a full copy, fix this latter 

data ConcurrentHashTable key val = ConcurrentHashTable {	
		  kvs :: IORef(Kvs key val)
}
--------------------------------------------------------------------------------------------------------------------------------------------

-- does not terminate if array is full, and key is not in it
-- TODO, use fitting hash function
getSlot :: forall key value . (Hashable key, Eq key) =>  
           Slots key value -> Mask -> key -> IO(Slot key value)
getSlot slots mask key =  do	let idx = hsh key mask
                                    newkey = K key 
				slot <- getSlt slots newkey idx mask
--collision treatment has to be done again on a write should the key cas fail
				return slot
		where hsh :: (Hashable key) => key -> Mask -> SlotsIndex
		      hsh k m = (hash k)  .&. m
		      full :: Key key -> Key key -> Bool
		      full  Kempty _ = False
		      full  k1 k2 = not (keyComp k1 k2)
		      getSlt:: Slots key value -> Key key -> SlotsIndex -> Mask -> IO(Slot key value)
		      getSlt slots newkey idx mask =
                        do let slot = (slots V.! idx) :: (Slot key value)
                           oldkey <- (readKeySlot slot)::IO(Key key)
                           slot <- (if full oldkey newkey
                                    then getSlt slots newkey (collision idx mask) mask
                                    else return slot) :: IO (Slot key value)
                           return slot --TODO count reprobes 

collision :: SlotsIndex -> Mask -> SlotsIndex
collision idx mask = (idx +1) .&. mask

maskHash :: Mask -> FullHash -> SlotsIndex
maskHash mask hsh = hsh .&. mask

unwrapValue :: Value val -> Maybe val
unwrapValue T = Nothing
unwrapValue Tp = Nothing
unwrapValue (V a) = Just a
unwrapValue (Vp a) = Just a
--TODO what if Sentinel

--compares keys
keyComp:: Eq key => 
          Key key -> Key key -> Bool
keyComp Kempty Kempty = True
keyComp Kempty _      = False
keyComp _     Kempty  = False
keyComp (K k1) (K k2) = k1 == k2 --TODO eqality on key what about hashes
--TODO does not cover primed keys


--compares the key in a slot with another key
keyCompSlot:: Eq key => 
          Slot key val-> Key key -> IO Bool
keyCompSlot slot key = do slotkey <- readKeySlot slot
		          return $ keyComp slotkey key

--for use by 
valCompComp :: Eq val =>
               ValComp val -> Value val -> Bool
valCompComp  (Left v1) v2 = valComp v1 v2
valCompComp  (Right MATCH_ANY) _ = True
--TODO does comparision with NO_MATCH_OLD fit


valComp :: Eq val => Value val -> Value val -> Bool
valComp (V a) (V b)= a == b
valComp T T = True
valComp T _ = False
valComp _ T = False
valComp S S = True
valComp S _ = False
valComp _ S = False
--TODO primed values

isPrimedValue :: Value val -> Bool
isPrimedValue Tp = True
isPrimedValue (Vp _) = True
isPrimedValue _ = False

isPrimedValComp :: ValComp val -> Bool
isPrimedValComp (Left v) = isPrimedValue v
isPrimedValComp (Right _) = False

isSentinel :: Value val -> Bool
isSentinel S = True
isSentinel _ = False


--puts an Sentinel into the slot
kill :: Slot key val -> IO ()
kill (Slot _ v) = writeIORef v S 


isTombstone :: Value val -> Bool
isTombstone T = True
isTombstone _ = False

isValue :: Value val -> Bool
isValue (V _) = True
isValue _ = False

isKEmpty :: Key key -> Bool
isKEmpty Kempty = True
isKEmpty _ = False

--TODO what if primed Tombstone





--see get
--reading during resize is already imlemented
get_impl :: (Eq key, Hashable key) => 
            ConcurrentHashTable key val -> Kvs key val -> key -> FullHash -> IO(Value val)
get_impl table kvs key fullhash = do let msk = mask kvs
                                         slts = slots kvs
				     slt <- getSlot  slts msk key --TODO pass fullhash
				     k <- readKeySlot slt
				     v <- readValueSlot slt
				     if keyComp k ( K key) --TODO are there primed keys
                                        then if isSentinel v  
						then do return $ assert $ hasNextKvs kvs 
							newkvs <- getNextKvs kvs
							get_impl table newkvs key fullhash --look in resized table
						else return v 
                                     	else return T  --TODO use hash-caching for keycompare
--TODO actually we could use IO(Maybe (Value val)) as return type
--TODO attention may return a primed value			
--TODO treat resize
--TODO count reprobes
--TODO only pass reference to table if necessary
--TODO fit get function with table resizing

--Accessing the slot
--------------------------------------------------------------------------------------------------------------------------------

readKeySlot:: Slot key value -> IO (Key key)
readKeySlot state = readIORef ( key state  )
		
readValueSlot:: Slot key value -> IO (Value value)
readValueSlot state = readIORef ( value state  )
	

--TODO compare means pointer equality, so get this fixed
--TODO Question When are 2 Keys equal, and why does ticket not require a to be in class eq, how exactly does the COMPARE part work
casKeySlot :: forall key value. (Eq key) =>
		 (Slot key value) -> Key key -> Key key -> IO ( (Bool, Key key) )
casKeySlot slt@(Slot ke va) old new = do
				sltold <- readIORef ke
				if not (keyComp sltold old) then return (False, sltold) --TODO Issue keyComp is not atomic 
					else do oldref <- (return ke)::IO(IORef (Key key))
						newref <- (newIORef new)::IO(IORef (Key key))
					        oldticket <- (readForCAS oldref) ::IO(Ticket(Key key))
					        newticket <- (readForCAS newref) ::IO(Ticket(Key key))
						(success, _) <- (casIORef2 ke oldticket newticket)::IO(Bool, Ticket(Key key))
						oldkey <- (return sltold)::IO(Key key) --TODO readIORef seems unnecessary
				        	if success then return (success, oldkey) else casKeySlot slt old new--TODO, is the repetition really necessary 
			--TODO compare old with key value if not equal return false, oldkey else cas oldkey if succes return true oldkey, if fail					

--TODO, see casKeySlot
casValueSlot :: forall key value. (Eq value) =>
	        (Slot key value) -> Value value -> Value value -> IO ( Bool, Value value )
casValueSlot slt@(Slot ke va) old new = do
				sltold <- readIORef va
				if not (valComp sltold old)  then return (False, sltold)
				   else do oldref <- return va
					   newref <- (newIORef new)::IO(IORef (Value value))
					   oldticket <- (readForCAS oldref) ::IO(Ticket(Value value))
					   newticket <- (readForCAS newref) ::IO(Ticket(Value value))
					   (success, _) <- (casIORef2 va oldticket newticket)::IO(Bool, Ticket(Value value))
 					   oldvalue <- return sltold
					   if success then return (success, oldvalue) else casValueSlot slt old new--TODO, is the repetition really necessary 

--setValueSlot :: forall key value. (Slot key value) -> Value value -> Value value -> IO ( Bool )




-------------------------------------------------------------------------------------------------------------------------

putIfMatch_T ::(Hashable key, Eq key, Eq value) =>
               ConcurrentHashTable key value -> key -> Value value -> ValComp value -> IO ( Value value)
putIfMatch_T table key putVal expVal = do let kvsref = kvs table
                                              ky = K key
                                          kv <- readIORef kvsref
                                          putIfMatch kv ky putVal expVal                                   


--TODO write during resize
--TODO refactor for readability/structure
--TODO, do we need to pass the Hashtable as parameter?
--TODO use only by acessor functions, not by resizing algorithm
--TODO assert key is not empty, putval is no empty, but possibly a tombstone, key value are not primed 
putIfMatch :: forall key val. (Hashable key, Eq key, Eq val) =>
              Kvs key val -> Key key -> Value val -> ValComp val -> IO (Value val)
putIfMatch kvs key putVal expVal = do
  let msk = mask kvs :: Mask
      slts = slots kvs
      fullhash = hash k :: FullHash
      K k = key
      idx = maskHash msk fullhash ::SlotsIndex
  --reprobe_cnt <- return 0
  --TODO get highest kvs and test if a resize is running and then get the second highest kvs
  return $ assert $ not $ isKEmpty key --TODO use eq TODO this is not in the original
  return $ assert $ not $ isPrimedValue putVal
  return $ assert $ not $ isPrimedValComp expVal
  slot  <- (getSlot slts msk k) ::IO(Slot key val)
  
  oldKey <-  readKeySlot slot
  if isKEmpty oldKey --if putvall TMBSTONE and oldkey == empty do nothing --TODO put this lines into an subfunction
    then if (isTombstone putVal)
         then return T {-TODO break writing value unnecessary -} 
         else ptIfmtch slts msk k fullhash putVal expVal idx newReprobeCounter --TODO remove line duplication
    else ptIfmtch slts msk k fullhash putVal expVal idx newReprobeCounter  
--TODO when would cas fail
     --actually does the putting after tests and special cases have been handled
     where ptIfmtch :: Slots key val -> Mask -> key -> FullHash -> Value val -> ValComp val ->
	             SlotsIndex -> ReprobeCounter -> IO(Value val)
           ptIfmtch slts msk key hsh newval compval idx reprobectr = do let slt = slts V.! idx
					                                    rekcall = ptIfmtch slts msk key hsh newval compval
								--TODO check if Slot is Kempty and compval==match any, then break and return T
				                                        keyfits <- helper2 slt
				                                        if keyfits 
					                                  then
					                                    do (success,ret) <- setval slt newval compval
									       if success then --was there an change made 
										  opSizeCntr ret newval else return ()
									       return ret
                                                                          else rekcall (collision idx msk)
											 (reprobectr +1)					
				-- checks if the key in slt fits or puts the newkey there if thers an empts
				where helper2 :: Slot key val -> IO Bool
				      helper2 slt = do (wasEmpty,_) <- casKeySlot slt Kempty (K key)
						       if wasEmpty then incSlotsCntr else return () 
--TODO doing a simple check before the expensive cas should not be harmfull, because of the monotonic nature of keys
						       if wasEmpty then return True else --TODO inc slots counter, inc size counter, except putvalue is a tombstone
--then ony slotscounter, or do the inc of size counter in the seval
							 keyCompSlot slt (K key) --simple key compare, TODO use fullhash
					--set the value, return old value
				      setval :: Slot key val -> Value val -> ValComp val -> IO((Bool,Value val)) --TODO define an datatype (Bool,Value val)
		       		      setval slt newval oldvalcmp = if isRight oldvalcmp then if (fromRight oldvalcmp) ==  MATCH_ANY then match_any slt newval
						else no_match_old slt newval 
					        else match slt newval (fromLeft oldvalcmp)
--TODO is there any backoff in cliff clicks algorithm
					where match_any :: Slot key val -> Value val -> IO((Bool,Value val))
					      match_any slt newval = do oldval <- readValueSlot slt
									if isTombstone oldval then return (False,oldval) else --is that actually ok if not done in order
										do (success, ret) <- casValueSlot slt oldval newval
										   if success then return (success,ret) else match_any slt newval
					      no_match_old :: Slot key val -> Value val -> IO((Bool,Value val))
					      no_match_old slt newval = do oldval <- readValueSlot slt --TODO do I actually need to do a cas here
									   if valComp oldval newval then return (False,oldval) else --not successfull
										do (success, ret) <- casValueSlot slt oldval newval
										   if success then (return (success,ret)) else no_match_old slt newval 
					      match :: Slot key val -> Value val -> Value val-> IO((Bool,Value val))
					      match slt newval oldval = do (success, ret) <- casValueSlot slt oldval newval
                                                                           return (success,ret)
						--TODO, do we need a cas here
				      --TODO if T to Value inc size counter, if V to T or S dec size counter
				      opSizeCntr :: Value val -> Value val -> IO()
				      --opSizeCntr old new
				      opSizeCntr T (V _) =incSizeCounter kvs
				      opSizeCntr T (Vp _) = incSizeCounter kvs
				      opSizeCntr T S = return ()
				      opSizeCntr (V _) (Vp _) = return ()
				      opSizeCntr (V _ )(V _) = return ()
				      opSizeCntr (Vp _) T =  decSizeCounter kvs
				      opSizeCntr (V _ ) T =  decSizeCounter kvs
				      opSizeCntr (V _)  S =  decSizeCounter kvs				
				      --TODO check witch changes are possible and witch arnt
				      --TODO save sizecntr operationd on resize

				      incSlotsCntr :: IO()
				      incSlotsCntr = incSlotsCounter kvs

--TODO add reprobe count			
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

--helper to acess first kvs
getHeadKvs :: ConcurrentHashTable key val -> IO(Kvs key val)
getHeadKvs table = do let kvsref= kvs table
		      readIORef kvsref

--gets then new resizedtable, throws error if does not exist
getNextKvs :: Kvs key val -> IO(Kvs key val)
getNextKvs kv = do let nwkvs = fromJust $ newkvs kv  --throws error
		   readIORef nwkvs 	  
--TODO change if structure of kvs changes

--Is a resize in progress?
hasNextKvs :: Kvs key val -> Bool
hasNextKvs kv =  isJust (newkvs kv )

--TODO possibly change return type to IO BOOL
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



containsValue :: (Eq val) => ConcurrentHashTable key val -> val -> IO(Bool)
containsValue table val = do let kvsref = kvs table
			     kv <- readIORef kvsref
			     containsVal kv (V val)
--TODO low priority
--TODO adopt if changes to data representation
--TODO adopt to resize


--TODO search should break off once found
containsVal :: forall key val. (Eq val) => Kvs key val -> Value val -> IO(Bool)
containsVal kvs val = do let slts = slots kvs
                         anyM (pred val) slts
			where
			pred :: Value val -> Slot key val -> IO(Bool)
			pred val slot = do sltkey <- readKeySlot slot 
					   sltval <- readValueSlot slot
					   if isKEmpty sltkey then return False else
						if valComp sltval val then return True else return False
						-- check if key is set (linearistion point for get is key AND value set) FIXME Primed values
			anyM :: forall m a. Monad m => (a -> m Bool) -> V.Vector a -> m Bool
			anyM test v =   V.foldM' g False v 
				where g :: Bool -> a -> (m Bool)
              			      g akk content = do testresult <- test content
				                         return $ testresult || akk
--TODO adopt to resizing, (by recursivly calling for newkvs) anyway what about primed, I should read that up
--TODO for this the linearisation point for inputing would be the cas on value even if the cas on key has not be done yet, actually its better to think about this for a while, maybe not export this function for a while
--TODO no reason anyM should not be inlined

put :: (Eq val,Eq key, Hashable key) => 
       ConcurrentHashTable key val -> key -> val -> IO( Maybe val)
put table key val = do old <- putIfMatch_T table key (V val) (Right NO_MATCH_OLD)
                       return $ unwrapValue old
--puts the value if there is no value matched to the key
putIfAbsent :: (Eq val,Eq key, Hashable key) => 
               ConcurrentHashTable key val -> key -> val -> IO( Maybe val)
putIfAbsent table key val = do old <- putIfMatch_T table key (V val) (Left T) --TODO is tombstone correct, what if there is a primed vaue 
			       return $ unwrapValue old

-- | Removes the key (and its corresponding value) from this map.
removeKey :: (Eq val, Eq key, Hashable key) =>
             ConcurrentHashTable key val -> key -> IO( Maybe val)
removeKey table key  = do old <- putIfMatch_T table key T (Right NO_MATCH_OLD)
		          return $ unwrapValue old

-- | Removes key if matched.
remove :: (Eq val, Eq key, Hashable key) =>
          ConcurrentHashTable key val -> key -> val -> IO( Bool)
remove table key val = do old <- putIfMatch_T table key T (Left (V val))
                          return $  (unwrapValue old) == Just val  


-- | do a put if the key is already mapped to some value
replace :: (Eq val, Eq key, Hashable key) =>
	   ConcurrentHashTable key val -> key -> val -> IO( Maybe val)
replace table key val = do old <- putIfMatch_T table key (V val) (Right MATCH_ANY)
                           return $ unwrapValue old

-- | do a put if the key is already mapped to the old value
replaceTest :: (Eq val, Eq key, Hashable key) =>
               ConcurrentHashTable key val -> key -> val -> val -> IO(Bool)
replaceTest table key newval oldval= do old <- putIfMatch_T table key (V newval) (Left (V oldval))
                                        return $  (unwrapValue old) == Just oldval 

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
-- In case inputSize is potency of 2 : identity
normSize:: Size -> Size
normSize inputSize = max min_size (sizeHelp inputSize 1)
	where sizeHelp :: Size -> Size -> Size
	      sizeHelp input size = if (size >= input) || (size == max_size)  then size else sizeHelp input (shiftL size 1)	
--FIXME TODO guard for Integer overun meaning some maximum size has to be set

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
		newSlot	:: IO(Slot key val)
		newSlot = do keyref <- newIORef Kempty
			     valref <- newIORef T     --TODO optimize somewhere, somewhat 
			     return $ Slot keyref valref 
		

--Debug code --TODO make inclusion conditional with preprocessor or something for DEBUG only
----------------------------------------------------------------------------------------------------------------------------------------------------

class DebugShow a where
        debugShow :: a -> IO String
--TODO Automatic indentation, ask on Stack Overflow about it.
--Debug print for the Hashtable
instance (Show k, Show v) => DebugShow (ConcurrentHashTable k v) where
        debugShow ht = do kvs <- getHeadKvs ht
                          str <- debugShow kvs 
                          return $  "ConcurrentHashtable:\n" ++ str

instance forall k v. (Show k, Show v) => DebugShow (Kvs k v) where
        debugShow kvs = debugshw 0 kvs
		where debugshw :: Int -> Kvs k v -> IO String
		      debugshw resizeCounter kvs = do str <- return $ "Kvs number " ++ (show resizeCounter) ++ " \n"
						      str <- return $ str ++  if isNothing $ newkvs kvs then "Is newest Kvs.\n" else "Is older Kvs.\n" 
						      maskstr <- debugShow $ mask kvs
						      str <- return $ str ++ maskstr
						      str <- return $ str ++ "sizeCounter:\n "
						      sizeCounterStr <- debugShow $ sizeCounter kvs
						      str <- return $ str ++ sizeCounterStr
						      str <- return $ str ++ "slotsCounter:\n "
						      slotsCounterStr <- debugShow $ slotsCounter kvs
						      str <- return $ str ++ slotsCounterStr ++ "Slots:\n"
						      slotsstr <- debugShow $ slots kvs
						      str <- return $ str ++ slotsstr
						      newKvsstr <-  if isNothing $ newkvs kvs then return "END.\n" else 
								do newkvsIORef <- return $ fromJust $ newkvs kvs
								   nwKvs <- readIORef newkvsIORef
								   debugShow nwKvs
						      return $ str ++ newKvsstr
  							

instance DebugShow Mask where 
        debugShow mask = do bitsstr <- return $ showIntAtBase 2 intToDigit mask ""
			    return $ "Mask: " ++ bitsstr ++ "\n"

--instance DebugShow SizeCounter where 
--        debugShow counter = undefined

--instance DebugShow SlotsCounter where 
--        debugShow counter = undefined

instance DebugShow AtomicCounter where
        debugShow counter = do number <- readCounter counter
			       return $ "Countervalue: " ++ (show number) ++ "\n" 
--TODO number the slots
instance forall k v. (Show k, Show v) => DebugShow (Slots k v) where
        debugShow slts = do str <- return $ "Vector length: " ++ (show $ V.length slts) ++ "\n"
                            V.foldM' g str slts where
				g :: String -> Slot k v -> IO String
				g akk slt = do sltstr <- debugShow slt
					       return $ akk ++ "Slot:\n" ++sltstr

instance (Show k, Show v) => DebugShow (Slot k v) where
        debugShow slt = do key <- readKeySlot slt
		           val <- readValueSlot slt
		           return $ "Key:\n" ++ (show key) ++ "\nValue:\n" ++ (show val) ++ "\n"

instance (Show k) => Show (Key k) where --TODO keys get primed
	show (K key) = "Key: " ++ (show key) 
	show Kempty  = "Key empty"

instance (Show v) => Show (Value v) where
	show (V val) = "Value: " ++ (show val)
	show (Vp val)= "Value (primed): " ++ (show val)
	show T = "Tombstone"
	show Tp = "Tombstone (primed)"
	show S = "Sentinel" 


-- Hashtable allows for telescopic resizes
-- returns number of ongoing resizes (number of tables -1)
getNumberOfOngoingResizes :: ConcurrentHashTable k v-> IO Int
getNumberOfOngoingResizes ht = do kvs <- getHeadKvs ht
                                  getNumber kvs
	where getNumber :: Kvs k v -> IO Int
              getNumber kvs = if isNothing $ newkvs kvs then return 0 else do nwkvs <- getNextKvs kvs
									      newkvsNumber <- getNumber nwkvs
									      return $ 1 + newkvsNumber
-- Hashtable allows for telescopic resizes
-- This means Values are stored somewhere in a list of vectors
-- returns length of every vector starting with the oldest
getLengthsOfVectors ::ConcurrentHashTable k v-> IO [Int]
getLengthsOfVectors ht = do kvs <- getHeadKvs ht
			    getLengths kvs
	where getLengths :: Kvs k v -> IO [Int]
              getLengths kvs = do slots <- return $ slots kvs  --FIXME could use let here
				  lngth <- return $ V.length slots
			          if not $ hasNextKvs kvs then return $ lngth:[] else do newkvs <- getNextKvs kvs
									   	         lst <- getLengths newkvs
									                 return $ lngth:lst 
