
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



-- TODO Data.HashTables... or Data.Concurrent...
--something like Data.HashTables.IO.NonBlocking.something
--               Data.HashTables.IO.Concurrent.NonBlocking.something
-- TODO make list of all Hashtable libraries in haskell and compare
module Data.HashTable.Concurrent 
	( 
	  -- * Alpha version limitations

          --
          -- * Maximum number of slots
          --
          -- A Hashtable can hold between __2^3__ and __2^31__ slots. 

          -- * Creating hash tables
          ConcurrentHashTable
	, newConcurrentHashTableHint, newConcurrentHashTable
          -- * Predicates and properties 
        , size, isEmpty, containsKey, containsValue
          -- * Basic reading and writing 
        , put, putIfAbsent, get 
          -- * Removing or replacing 
        , removeKey, remove, replace, replaceTest, clear, clearHint -- TODO should clearHint really be exported

	  -- * Debuging
	, debugShow, getNumberOfOngoingResizes, getLengthsOfVectors, getSlotsCounters, countUsedSlots, getReprobeCount
        )
	where

import GHC.IORef(IORef(IORef), readIORef, newIORef, writeIORef)
import Data.Hashable(Hashable, hash)
import Data.Bits((.&.), xor, shiftL, shiftR) 
import Data.Atomics
--todo restrict and qualify
import Control.Exception(assert)
import Data.Atomics.Counter
import qualified Data.Vector as V
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Either.Unwrap (fromLeft, isLeft, isRight, fromRight)
import Control.Monad.ST (runST)
import Data.Word (Word) 
import Data.Int (Int64)
import Control.Monad (forM)

import Control.Exception.Base (ioError) -- TODO remove once resizing works
import System.IO.Error (userError)      -- TODO ditto

import Numeric (showIntAtBase) -- FIXME for debug only
import Data.Char (intToDigit)       --dito

import Test.QuickCheck.Arbitrary as QCA
import Test.QuickCheck.Gen as QCG
import Test.QuickCheck.Gen.Unsafe as QCGU

import Data.Time.Clock (getCurrentTime, UTCTime, NominalDiffTime, DiffTime, secondsToDiffTime, diffUTCTime)
import Control.Concurrent (threadDelay)

import Math.NumberTheory.Logarithms (intLog2)

import Control.Exception (assert) -- Assert

-- TODO look for 32/64 bit issues, Magic Numbers

-- Time
-------------------------------------------------------------------------------------------------------------------------
setLastResizeTime :: ConcurrentHashTable key value -> IO ()
setLastResizeTime table = do let timeref = timeOfLastResize table
			     timestamp <- getCurrentTime 
			     writeIORef timeref timestamp-- TIMETODO

type Time = UTCTime -- ^ time in milliseconds  -- TIMETODO
type Timespan = NominalDiffTime -- TIMETODO

timediff = diffUTCTime


getTime :: IO Time
getTime = getCurrentTime

getLastResizeTime :: ConcurrentHashTable key value -> IO Time
getLastResizeTime table = do let timeref = timeOfLastResize table
			     readIORef timeref
------------------------------------------------------------------------------------------

min_size_log = 3
min_size = 2 ^ min_size_log --must be power of 2, compiler should turn this into a constant --TODO have this as a perprocessor constant, so it 
--can be included in haddock

max_size_log = 31
max_size = 2 ^ max_size_log

resize_timespan:: Timespan-- TIMETODO 
resize_timespan = realToFrac $ secondsToDiffTime 1 -- FIXME question 1 second or 10 seconds
-- TODO put this value in the haddock documentation

-- FIXME whats the defined behaviour, if the hashtable gets really full, better, to throw an error, than to somehow fail or hang
-- TODO is there an suitable error to be thrown

_reprobe_limit = 10

reprobe_limit :: Int -> Int
reprobe_limit len = _reprobe_limit + (shiftR len 2)

getMask:: Size -> Mask
getMask size = size -1 

--data representation
---------------------------------------------------------------------------------------------------------------------------------
-- Kempty : empty, K : neverchanging key
--data Key key = Kempty | K key deriving (Eq) 
  -- TODO make instance of Eq 
  -- TODO do keys need to be primed
data Key k = Kempty 
           | Key { fullHash :: !FullHash
		 , keyE :: !k
		 } 

-- T : empty, tombstone, Tp : tombstone primed, V : value, Vp : value primed -- TODO need tombstoes to be primed
data Value value =  T | Tp | V value | Vp value | S deriving (Eq) -- TODO what kind of comparision is used



data Slot k v =   Slot {
				key :: IORef (Key k)
				, value :: IORef (Value v)
				}


data Kvs k v =   Kvs {
	newkvs :: IORef (Maybe ( Kvs k v))  
  -- TODO, get rid of the extra indirection, 
  --   https://github.com/gregorycollins/hashtables/blob/master/src/Data/HashTable/Internal/UnsafeTricks.hs 
  -- TODO but this trick should already be in the Maybe Monad, if I do this I should make it general for all the indirections 
  -- TODO, is there some IORef Maybe module or even moe general around, if not I could write one, or should ghc be able to optimize this.
	,slots :: Slots k v 
	, mask :: Mask
	, slotsCounter :: SlotsCounter
	, sizeCounter :: SizeCounter
	, _copyDone :: IORef CopyDone
	, copyIndex :: IORef CopyIndex
	, resizers :: IORef Resizers  
	
} 

type SlotsCounter = AtomicCounter 
type SizeCounter  = AtomicCounter 

type FullHash = SlotsIndex
type CopyDone = SlotsIndex -- originaly an atomic-long-field updater
type CopyIndex = SlotsIndex
type FullHashUnsigned = Word
type SlotsIndex = Int
type Mask = SlotsIndex
type Size = Int
type SizeLog = Int

type Resizers = Int


type ReprobeCounter = Int

newReprobeCounter = 0

data ValComparator = MATCH_ANY | NO_MATCH_OLD deriving Eq

type ValComp val = Either (Value val) ValComparator 

type ReturnValue val = (Bool, Value val) 
   -- TODO find good name for this type, write description

type Slots key val = V.Vector (Slot key val)
  -- TODO issue accessing the array generates a full copy, fix this latter 

data ConcurrentHashTable key val = ConcurrentHashTable {	
		  kvs :: IORef(Kvs key val)
		  , timeOfLastResize :: IORef Time
}

-- functions for keys
--------------------------------------------------------------------------------------------------------------------------------------------


newKey :: Hashable k => k -> Key k
newKey k = Key (spreadHash $ hash k) k

emptyKey = Kempty


getFullHash :: Key k -> FullHash
getFullHash = fullHash

getKey :: Key k -> Maybe k
getKey Kempty = Nothing
getKey (Key h k) = Just k

--compares keys
keyComp:: Eq key => 
          Key key -> Key key -> Bool
keyComp Kempty Kempty = True
keyComp Kempty _      = False
keyComp _     Kempty  = False
keyComp (Key h1 k1) (Key h2 k2) = if h1 == h2 then k1 == k2 else False  

isKEmpty :: Key key -> Bool
isKEmpty Kempty = True
isKEmpty _ = False


-- functions for values
--------------------------------------------------------------------------------------------------------------------------------------------
unwrapValue :: Value val -> Maybe val
unwrapValue T = Nothing
unwrapValue Tp = Nothing
unwrapValue (V a) = Just a
unwrapValue (Vp a) = Just a
-- TODO what if Sentinel
--probably best to throw error

--for use by 
valCompComp :: Eq val =>
               ValComp val -> Value val -> Bool
valCompComp  (Left v1) v2 = valComp v1 v2
valCompComp  (Right MATCH_ANY) _ = True
-- TODO does comparision with NO_MATCH_OLD fit




valComp :: Eq val => Value val -> Value val -> Bool
valComp (V a) (V b)= a == b
valComp T T = True
valComp T _ = False
valComp _ T = False
valComp S S = True
valComp S _ = False
valComp _ S = False
-- TODO primed values

isPrimedValue :: Value val -> Bool
isPrimedValue Tp = True
isPrimedValue (Vp _) = True
isPrimedValue _ = False

isPrimedValComp :: ValComp val -> Bool
isPrimedValComp (Left v) = isPrimedValue v
isPrimedValComp (Right _) = False

primeValue :: Value val -> Value val
primeValue (V v) = Vp v

isSentinel :: Value val -> Bool
isSentinel S = True -- FIXME
isSentinel Tp = True -- FIXME Sentinel is probably the same as Tombstone primed 
isSentinel _ = False


--puts an Sentinel into the slot
kill :: Slot key val -> IO ()
kill (Slot _ v) = writeIORef v S 


isTombstone :: Value val -> Bool
isTombstone T = True
isTombstone _ = False

isUntoucedValue :: Value val -> Bool
isUntoucedValue = isTombstone

isValue :: Value val -> Bool
isValue (V _) = True
isValue _ = False

-- TODO what if primed Tombstone
--------------------------------------------------------------------------------------------------------------------------------------------

-- does not terminate if array is full, and key is not in it
-- TODO possibly return whether the slot has a key or the key is empty
-- TODO testcases for reprobe
-- FIXME does not terminate if reprobes infinitly or very often, best would be to return an Maybe
-- | Reprobes until it find an Slot with a fitting or empty key
getSlot :: forall key value . (Eq key) =>  
           Slots key value -> Mask -> Key key -> IO(Slot key value, ReprobeCounter)
getSlot slots mask key =  do let fllhash = fullHash key
				 idx = maskHash mask fllhash  
			     getSlt slots newReprobeCounter key idx mask
		where full :: Key key -> Key key -> Bool
		      full  Kempty _ = False
		      full  k1 k2 = not (keyComp k1 k2) --Collison threatment is done again whe CASing key
		      getSlt:: Slots key value -> ReprobeCounter -> Key key -> SlotsIndex -> Mask -> IO(Slot key value, ReprobeCounter)
		      getSlt slots rpcntr newkey idx mask =
                        do let slot = (slots V.! idx) :: (Slot key value)
                           oldkey <- (readKeySlot slot)::IO(Key key)
                           if full oldkey newkey 
                                       then getSlt slots (incReprobeCounter rpcntr) newkey (collision idx mask) mask -- Reprobe
                                       else return (slot,rpcntr) -- Found a Slot that has eiter an fitting or an empty key
                           

collision :: SlotsIndex -> Mask -> SlotsIndex
collision idx mask = (idx +1) .&. mask

maskHash :: Mask -> FullHash -> SlotsIndex
maskHash mask hsh = hsh .&. mask



-- | variant of single word Wang/Jenkins Hash
-- see line 262 <https://github.com/boundary/high-scale-lib/blob/master/src/main/java/org/cliffc/high_scale_lib/NonBlockingHashtable.java>
spreadHash :: FullHash -> FullHash   
spreadHash input = runST $ do h <- return $ input + ( (shiftL input 15) `xor` 0xffffcd7d)
			      h <- return $ h `xor` (unsignedShiftR h 10)
			      h <- return $ h + (shiftL h 3) 
			      h <- return $ h `xor` (unsignedShiftR h 6)
			      h <- return $ h + (shiftL h 2) + (shiftL h 14)
			      return $ h `xor` (unsignedShiftR h 16)
	where unsignedShiftR :: Int -> Int -> Int
	      unsignedShiftR input len= fromIntegral (shiftR ((fromIntegral input)::FullHashUnsigned) len) 





-- | Compares the key in a slot with another key
keyCompSlot:: Eq key => 
          Slot key val-> Key key -> IO Bool
keyCompSlot slot key = do slotkey <- readKeySlot slot
		          return $ keyComp slotkey key



isKEmptySlot :: Slot key val -> IO Bool
isKEmptySlot slot = do slotkey <- readKeySlot slot
		       return $ isKEmpty slotkey


-- see 'get'
-- reading during resize is already imlemented
get_impl :: (Eq key, Hashable key) => 
            ConcurrentHashTable key val -> Kvs key val -> Key key  -> IO(Value val)
get_impl table kvs key          = do let msk = mask kvs
                                         slts = slots kvs
				     (slt,_) <- getSlot  slts msk key
				     k <- readKeySlot slt
				     v <- readValueSlot slt
				     if keyComp k key
                                        then if (isSentinel v) ||  (isPrimedValue v)  -- FIXME what happens in case of primed value
						then do ass <- hasNextKvs kvs
							return $ assert ass 
							newkvs <- getNextKvs kvs
							get_impl table newkvs key  --look in resized table
						else return v 
                                     	else return T  
-- TODO actually we could use IO(Maybe (Value val)) as return type
-- TODO attention may return a primed value			
-- TODO treat resize -- TODO call helpCopy
-- TODO count reprobes
-- TODO only pass reference to table if necessary
-- TODO fit get function with table resizing

-- Accessing the slot
--------------------------------------------------------------------------------------------------------------------------------

-- does not wrap around the idx according to mask

kvsSlot :: Kvs key value -> SlotsIndex -> Slot key value
kvsSlot kvs idx = (slots kvs)  V.! idx
	

readKeySlot:: Slot key value -> IO (Key key)
readKeySlot state = readIORef ( key state  )
		
readValueSlot:: Slot key value -> IO (Value value)
readValueSlot state = readIORef ( value state  )

readSlot :: Slot key value -> IO (Key key, Value value)
readSlot slt = do key <- readKeySlot slt
		  value <- readValueSlot slt
		  return (key, value)
	
-- | CAS the slot from KEmpty to the new key and succeeds
-- , does nothing and succeeds if the new key is already in the slot
-- , does nothing and fails if another key is already in the slot
-- , never changes the slot if there is already a key in the slot
-- , returns pair of Bool: 1.: True -> new key is in the slot, False -> some other key is in the slot (use this for collision detection)
-- ,                       2.: True -> cased the slot of KEmpty to the new key thereby using up the slot, False -> the new key was already 
--					in the slot ( use this for adapting the slotscounter) 
casKeySlot :: (Eq key) =>
	(Slot key value) -> Key key -> IO (Bool,Bool)
casKeySlot (Slot ke _) new = do return $ assert $ not $ isKEmpty new -- new key is a actuall key, not KEmpty
				oldticket <- readForCAS ke 
				oldkey <- return $ peekTicket oldticket
				if not $ isKEmpty $ oldkey then if keyComp new oldkey  then return (True,False) else return (False,False) else
				-- is there already an key in the slot, if so is it the same as the new key
					 do (success, retkeyticket) <- casIORef ke oldticket new
					    retkey <- return $ peekTicket retkeyticket
					    return $ assert $ (success && (keyComp new retkey)) || -- cas succeds
						   ((not success) && (not $ isKEmpty retkey)) -- cas only fails if there is already an key
					    if success then return (True,True) else
						   if keyComp new retkey then return (True,False) else return (False,False)
						   -- there was already an key in the slot, no retry because slotkeys are never to be overwritten
						   -- is it the same as the new key  	
 									 								

-- | CAS the slot to the new value; if the slot value fits the compare value
--   returns success and the old value.
casValueSlot :: forall value key. (Eq value) =>
	(Slot key value) -> ValComp value -> Value value -> IO (Bool, Value value)
casValueSlot slt@(Slot _ va) cmpvaluecomp newvalue = do
	oldvalueticket <- readForCAS va
	oldvalue <- return $ peekTicket oldvalueticket
	return $ assert $ (not $ isSentinel oldvalue) && (not $ isPrimedValue oldvalue) --FIXME resize
	if not $ matchesVal oldvalue cmpvaluecomp newvalue then return (False, oldvalue) else do  -- TODO then else confused
		(success, retticket) <- casIORef va oldvalueticket newvalue 
		if success then return (True, oldvalue) else casValueSlot slt cmpvaluecomp newvalue -- TODO This is the Only place for backoff code 
  where matchesVal :: Value value -> ValComp value -> Value value -> Bool
	matchesVal oldvalue (Right MATCH_ANY) _ = not $ isTombstone oldvalue --oldvalue is not a tombstone  --FIXME, RESIZE related PRIMED,SENTINEL 
	matchesVal oldvalue (Right NO_MATCH_OLD) newvalue = not $ valComp oldvalue newvalue -- newvalue != oldvalue 	
	matchesVal oldvalue (Left cmpvalue) _ = valComp oldvalue cmpvalue --oldvalue == cmpvalue --FIXME, RESIZE related PRIMED,SENTINEL
--matchesVal is resize ignorant
-- TODO lets think about how to threat primed values

-- TODO one could save oneself one readForCas by reusing retticket, that why the thing returns a ticket.
--by havin casValueSlot as an wraper for an rekursive functionusing tickets



casStripPrime :: (Slot key value) -> IO ()
casStripPrime slt@(Slot _ va) = do oldticket <- readForCAS va
				   oldvalue <- return $ peekTicket oldticket
				   if not $ isPrimedValue oldvalue then return () else
				     do (_,_) <- casIORef va oldticket (stripPrime oldvalue)
					return ()
	where stripPrime :: Value val -> Value val
	      stripPrime (Vp a) = V a
	      stripPrime Tp = T
	      stripPrime unprimed = unprimed
               --get a reference to the value, 
               -- if not a prime, then write has already happend -> end
               -- else contruct an unprimed value, and a reference to it
               -- cas that agaist the original reference if failed because somebody already wrote an value then ->end

            


--resizes stuff
-----------------------------------------------------------------------------------------------------------------------



--
helpCopy :: forall key value . (Hashable key, Eq key, Eq value) => 
	ConcurrentHashTable key value ->  IO ()
helpCopy ht  = do topkvs <- getHeadKvs ht
		  resizeinprogress <- hasNextKvs topkvs -- if a resize starts after this test its like the function was called before the start of the resize
		  if not resizeinprogress then return () else helpCopyImpl ht topkvs False
	where helpCopyImpl :: ConcurrentHashTable key value -> Kvs key value -> Bool -> IO ()
	      helpCopyImpl ht oldkvs copyall =  let minCopyWork = min 1024 $ getLength oldkvs 
						    oldlen = getLength oldkvs
						in
						do newkvs <- getNextKvs oldkvs -- TODO assert hasNextKvs oldkvs == return True
						   helpCopyLoop ht oldkvs newkvs  oldlen copyall minCopyWork (-9999) (-1)	-- TODO initial parameter for copy idx
						   copyCheckAndPromote ht oldkvs 0
	      helpCopyLoop :: ConcurrentHashTable key value -> Kvs key value -> Kvs key value ->  Size -> Bool-> Size -> Size -> Size-> IO () --TODO chose fitting synonym for int 
	      helpCopyLoop ht oldkvs newkvs  oldlen copyall minCopyWork copyidx panicStart =  
	       do copydone <- getCopyDone oldkvs 
		  if copydone >= oldlen then return () else do    
		    -- TODO set panic_start -- TODO add copyidx what about copydone it is shared
		   (copyidx,panicStart) <- if panicStart /= (-1) then return (copyidx,panicStart) else do 
			-- TODO set copyidx using cas 
			cpyidx <- casIncCopyIndex oldkvs oldlen copyidx -- TODO Question is it really oldkvs 
		   	pncStrt <- if cpyidx < (shiftL oldlen 1) then return  panicStart else return cpyidx  -- TODO is the right copyidy returned
			return (cpyidx,pncStrt)

		   lstwd <- forM [0..minCopyWork] (\x -> copySlot ht (maskHash (mask oldkvs) (copyidx+x) ) oldkvs newkvs) -- actual copying -- TODO index
		   workdone <- return $ sum $ map fromEnum lstwd -- sum lstwd
		   if workdone > 0 then copyCheckAndPromote ht oldkvs workdone else return () -- TODO if workdone > 0 copy check and promote
		-- increment the local version of copyidx
		   if (not copyall) && (panicStart == (-1)) then return () else	 
		   	   helpCopyLoop ht oldkvs newkvs  oldlen copyall minCopyWork (copyidx + minCopyWork) panicStart --TODO process copyidx panicStart
	
-- TODO enter calls of helpCopy in the marked functions

resizeInProgress :: ConcurrentHashTable key value -> IO Bool
resizeInProgress ht = do topkvs <- getHeadKvs ht
			 hasNextKvs topkvs



-- TODO, what happens to the slotscounter of the old kvs, how does the program know, that
-- the old kvs has been completely copied
copyOnePair :: Slot key value -> Kvs key value -> IO () 
copyOnePair slt newkvs = do undefined -- TODO read Slot 
                                      -- TODO should there be any assert special case
                                      -- threatment, or should these be in a wraper
			    (oldkey, oldvalue) <- readSlot slt -- TODO no Key here -> nothing to copy
			    -- TODO Tombstone no need to copy, but best to cas a sentinel
			    (casNewSuccess, newSlot) <- putAndReturnSlot oldkey oldvalue newkvs  -- the slot on the newkvs where the primed value has been put)
			    if casNewSuccess then do  casSsuccess <- undefined 
		            			      if casSsuccess then do fence 
						                             casStripPrime newSlot
							else undefined
				else undefined
	where fence = undefined
	      putAndReturnSlot :: Key key -> Value value -> Kvs key value -> IO (Bool, Slot key value)
	      putAndReturnSlot key val kvs = undefined
    
-- TODO is the same as copyOnePair? compare type signature
copySlot :: forall key value . (Hashable key, Eq key, Eq value)=> 
	ConcurrentHashTable key value -> SlotsIndex -> Kvs key value -> Kvs key value -> IO Bool
copySlot ht idx oldkvs newkvs = do let oldslot = kvsSlot oldkvs idx
				       newslot = kvsSlot newkvs idx-- TODO we assume that idx is always in bound for oldkvs, and that newkvs is also larger than oldkvs TODO write this into an assertion
				   ky <- readKeySlot oldslot
				   (finished,copied,oldvalueunprimed) <- primeOldValue oldslot
				   if finished then return copied else do
				   	oldnewkvsvalue <- putIfMatch newkvs ky oldvalueunprimed (Left T) -- put in unused slot
				   	copied <- return $ isUntoucedValue  oldnewkvsvalue  
				   	slamValue oldslot
				   	return copied
	where slamValue :: Slot key value -> IO ()
	      slamValue slt = do oldval <- readValueSlot slt
				 (suc, oldval) <- casValueSlot slt (Left oldval) Tp
				 if suc then return () else slamValue slt -- TODO this could possibly be done with a simple write but then migh require an implicit fence
	      primeOldValue :: Slot key value -> IO (Bool,Bool,Value value)
	      primeOldValue slot = do val <- readValueSlot slot
				      if isPrimedValue val && val == Tp then return (True,False,Tp) else
					if isPrimedValue val && val /= Tp then return (False,False,val) else
						do let primedval = primeValue val
						   (suc,_) <- casValueSlot slot (Left val) primedval -- cas value
						   if not suc then primeOldValue slot else 
							   if primedval == Tp then return (True,True,Tp) else return (False, False, primedval) -- return depending on wether tp, TODO put this into an loop
	      primeValue :: Value value -> Value value
	      primeValue (V val) = Vp val
	      primeValue T     = Tp 
	      primeValue _     = undefined -- TODO what happens on Sentinels, assert input is not primed	
-- TODO slam key of oldslot for minor speed optimisation
-- TODO prime old value
-- TODO copy into new
-- TODO tombprime oldvalue 



-- | removes the oldest kvs from the ht
--throws error, if there is no resize in progress and thus only one kvs
--some other routine has to determine that the oldest kvs is completly copied, and that the routine is not called multiple times for the same kvs
-- maybe there has to be an cas used -- TODO probably better us CAS with 'casHeadKvs'
removeOldestKvs :: ConcurrentHashTable key val -> IO ()
removeOldestKvs ht = do let htKvsRef = kvs ht
			oldestKvs <- getHeadKvs ht
			secondOldestKvs <- getNextKvs oldestKvs   --throws error, if no resize is in progress        
			writeIORef htKvsRef secondOldestKvs
			--oldestKvs will be GCted, one could explicitly destroy oldestKvs here

copySlotAndCheck :: forall key value . (Hashable key, Eq key, Eq value) => 
	ConcurrentHashTable key value -> Kvs key value -> SlotsIndex -> Bool -> IO () --(Kvs key value) -- TODO make type signature
copySlotAndCheck  ht oldkvs idx shouldHelp = do newkvs <- getNextKvs oldkvs -- TODO originally theres a volatile read
						success <- copySlot ht idx  oldkvs newkvs 
						if success then copyCheckAndPromote ht oldkvs 1 else return ()
						if not shouldHelp then return () else  -- TODO possible return newkvs here
							helpCopy ht -- TODO does helpCopy need any further parameters


-- | Increases the copy done counter for oldkvs by workdone, and removes oldkvs if it is the oldest kvs and has been fully copied
copyCheckAndPromote :: ConcurrentHashTable key value -> Kvs key value -> SlotsIndex -> IO ()
copyCheckAndPromote ht oldkvs workdone  = do let oldlen = getLength oldkvs   -- I do think I should get the ticket befor this function is called so we can check simply if the thing that we idetified as headkvs is still the headkvs, or something
					     copydone <- if workdone > 0 then casCopyDone oldkvs workdone else getCopyDone oldkvs
					     isheadkvs  <- isHeadKvs ht oldkvs-- TODO
					     if copydone < oldlen then return () else if not $ isheadkvs  then return () else
						do newkvs <- getNextKvs oldkvs -- Assert hasNextKvs
						   casHeadKvs ht oldkvs newkvs
						   setLastResizeTime ht
-- TODO assert workdone is posetive
-- TODO 1. if workdone > 0 casCopyDone.
--      2. test if new value of copyDon == oldlen
--      3. if table has been fully copyied
--          if oldkvs == topKvs then casKVS with newKvs
              --set last resize milli
-- TODO, how to determine if kvs is topkvs 

-- TODO write a casRoutine for copydone

-- TODO only use in copyCheckAndPromote
casCopyDone :: Kvs key value -> SlotsIndex -> IO SlotsIndex
casCopyDone kv workdone = do copydoneref <- (return $ getCopyDoneRef kv)::(IO(IORef CopyDone)) -- TODO get copydone object  -- Note copy done is originaly an
--atomicLongFieldUpdater -- TODO have copydone its own type
		 	     copydoneticket <- readForCAS copydoneref
			     casCD copydoneref copydoneticket workdone
 	where casCD cdref ticket workdone = do let newval = (peekTicket ticket) + workdone
					       (success, retticket) <- casIORef cdref ticket newval
					       if success then return newval else casCD cdref retticket workdone
-- TODO assert copydone + workdone <= oldlen, workdone > 0

getCopyDone :: Kvs key value -> IO CopyDone
getCopyDone kvs = do let ref = _copyDone kvs
		     readIORef ref 

getCopyDoneRef :: Kvs key value -> IORef CopyDone
getCopyDoneRef = _copyDone

resize :: ConcurrentHashTable key value -> Kvs key value -> IO (Kvs key value)
resize ht oldkvs= do hasnextkvs <- hasNextKvs oldkvs -- TODO clean up the code
		     if  hasnextkvs then do newkvs <- getNextKvs oldkvs
					    return newkvs
			else do
			  let sltscntr = slotsCounter oldkvs
			      szcntr = sizeCounter oldkvs
			      sz = getLength oldkvs
			  oldtime <- getLastResizeTime ht
			  newtime <- getTime
			  newsz <- heuristicNewSize sz szcntr oldtime newtime sltscntr
			  mgs <- return $ megs $ log2size newsz  -- binary log of newsize
			  res <- incCASResizers oldkvs
			  if not ((res >= 2) && (mgs > 0)) then return () else do 
				hasnextkvs <- hasNextKvs oldkvs
	                        if  hasnextkvs then do newkvs <- getNextKvs oldkvs
				                       return () -- FIXME this results in another test befor terminating
				  else threadDelay (8*mgs) -- sleep for milliseconds TODO compilers other than ghc
			  hasnextkvs <- hasNextKvs oldkvs
	                  if  hasnextkvs then do newkvs <- getNextKvs oldkvs
			                         return newkvs
						else do     newkvs <- newKvs newsz szcntr
						            hasnextkvs <- hasNextKvs oldkvs
		     					    if  hasnextkvs then getNextKvs oldkvs
								else casNextKvs oldkvs newkvs
				-- 
			  -- TODO compute log of newsz -- TODO for what purpose?
			  -- TODO casinc resizer count -- TODO create an appropriate field in kvs  todo why the resizers field
			  -- TODO backoff if already lots of resizers
			  -- TODO test again if resize already in progress -- TODO why?
			  -- TODO create and cas newkvs todo test if already done again
			  -- TODO other copy stuff	
	where  heuristicNewSize:: Size -> SizeCounter -> Time -> Time -> SlotsCounter -> IO Size -- TIMETODO
	       heuristicNewSize len szcntr oldtime newtime sltcntr = do sz <- readCounter szcntr
									slts <- readCounter sltcntr
									newsze <- return sz
									-- TODO assert len is posetive
									newsze <- return $ if sz >= (shiftR len 2) then
									   if sz >= (shiftR len 1) then  shiftL len 2 else shiftL len 1  else newsze
									newsze <- return $ if (newsze <= len) 
									  && ((timediff newtime   oldtime) <= resize_timespan) -- TIMETODO
									  && (slts >= (shiftL sz 1))  
									  then shiftL len 1 else newsze
									newsze <- return $ if newsze < len then len else newsze
									return $ normSize newsze-- TODO assert table is not shrinking
-- TODO more functional coding, or at least seperate ST, IO, 
	       megs :: Int -> Int -- TODO newtypes possibly
	       megs log2 = shiftR (shiftL ((shiftL (shiftL 1 log2) 1) + 4) 3) 20 -- TODO replace magic numbers with constant -- FIXME this is for the java version haskell version has different memory layout; at the moment this should be bigger
	       getResizers :: Kvs key value -> IO Resizers
	       getResizers kvs = do let resref = resizers kvs -- TODO do we need this function
			            readIORef resref
	       incCASResizers :: Kvs key value -> IO Resizers
	       incCASResizers kvs = do let resref = resizers kvs
				       ticket <- readForCAS resref
				       incR ticket resref
		 where	incR :: Ticket Resizers-> IORef Resizers-> IO Resizers
			incR tic ref = do let val = peekTicket tic
					  (success, oldtic) <- casIORef ref tic (val+1)
					  if success then return val else incR oldtic ref
				       -- TODO any reason not to use atomicCounter?

-- TODO add resize to putIfMatch

-- TODO add tableFull to putIfMatch
-- TODO Note, for performance sake an implementation of SlotsCounter that is only aproximatly accurate would fully suffice 
-- | Heuristic to determine whether the kvs is so full, that an resize is recommended
tableFull :: ReprobeCounter  -- ^ just to check if a resize s in order because of to many reprobes anyway
	     -> Size         -- ^ the number of slots in the kvs 
	     -> SlotsCounter -- ^ how many of them are in use 
	     -> IO Bool
tableFull recounter len sltcounter = do sltcn <- readCounter sltcounter
					return $ recounter >= _reprobe_limit && --always allow a few reprobes
				                 sltcn >= (reprobe_limit len)   -- kvs is quarter full


-------------------------------------------------------------------------------------------------------------------------

putIfMatch_T ::(Hashable key, Eq key, Eq value) =>
               ConcurrentHashTable key value -> key -> Value value -> ValComp value -> IO ( Value value)
putIfMatch_T table key putVal expVal = do let ky = newKey key
                                          kv <- getHeadKvs table
                                          putIfMatch kv ky putVal expVal                                   


-- TODO write during resize
-- TODO call helpCopy, resize, tableFull
-- TODO refactor for readability/structure
-- TODO add reprobe counter
-- TODO, do we need to pass the Hashtable as parameter?
-- TODO use only by acessor functions, not by resizing algorithm
-- TODO assert key is not empty, putval is no empty, but possibly a tombstone, key value are not primed 
putIfMatch :: forall key val. (Hashable key, Eq key, Eq val) =>
              Kvs key val -> Key key -> Value val -> ValComp val -> IO (Value val)
putIfMatch kvs key putVal expVal = do
  let msk = mask kvs :: Mask
      slts = slots kvs
      fllhash = fullHash key  :: FullHash 
      idx = maskHash msk fllhash ::SlotsIndex --parameterise maskHash with kvs and key
 
  -- TODO get highest kvs and test if a resize is running and then get the second highest kvs
  return $ assert $ not $ isKEmpty key -- TODO this is not in the original
  return $ assert $ not $ isPrimedValue putVal
  return $ assert $ not $ isPrimedValComp expVal
  (slot, rpcntr)  <- (getSlot slts msk key) ::IO(Slot key val, ReprobeCounter) --TODO, either remove this or have it give back a reprobe counter
  
  oldKey <-  readKeySlot slot
  if isKEmpty oldKey --if putvall TMBSTONE and oldkey == empty do nothing -- TODO put this lines into an subfunction
    then if ((isTombstone putVal) || expVal == Right MATCH_ANY || if isLeft expVal then not $ isTombstone $ fromLeft expVal else False)
 -- if oldkey empty and MATCH_ANY do nothing
         then return T {-TODO break writing value unnecessary -} 
         else ptIfmtch slts msk key putVal expVal idx rpcntr -- TODO remove line duplication
    else ptIfmtch slts msk key putVal expVal idx rpcntr  
-- TODO when would cas fail
     --actually does the putting after tests and special cases have been handled
     where ptIfmtch :: Slots key val -> Mask ->  Key key  -> Value val -> ValComp val ->
	             SlotsIndex -> ReprobeCounter -> IO(Value val)
           ptIfmtch slts msk key  newval compval idx reprobectr = do let slt = slts V.! idx
					                                 rekcall = ptIfmtch slts msk key newval compval
								-- TODO check if Slot is Kempty and compval==match any, then break and return T
				                                     keyfits <- helper2 slt
				                                     if keyfits 
					                                  then
					                                    do (success,ret) <- casValueSlot slt compval newval
									       if success then --was there an change made 
										  opSizeCntr ret newval else return ()--updating the sizecounter
									       return ret
                                                                          else rekcall (collision idx msk)
											 (reprobectr +1)					
				-- checks if the key in slt fits or puts the newkey there if thers an empts
				-- responsible for updating the slotscounter
				where helper2 :: Slot key val -> IO Bool
				      helper2 slt = do (success, cased) <- casKeySlot slt key
						       if cased then incSlotsCntr else return () 
-- TODO doing a simple check before the expensive cas should not be harmfull, because of the monotonic nature of keys
						       return success 		 
				      -- TODO if T to Value inc size counter, if V to T or S dec size counter
				      opSizeCntr :: Value val -> Value val -> IO()
				      opSizeCntr T (V _)      = incSizeCounter kvs
				      opSizeCntr T (Vp _)     = incSizeCounter kvs -- TODO debatable Primes are used for 2 stage copy so I need a detialed plan on how to count size during copys, in effect once a key val pair becomes available size increases it becomes unavailiable decreases
				      opSizeCntr T S          = return ()
				      opSizeCntr (V _) (Vp _) = return ()
				      opSizeCntr (V _ )(V _)  = return ()
				      opSizeCntr (Vp _) T     =  decSizeCounter kvs
				      opSizeCntr (V _ ) T     =  decSizeCounter kvs
				      opSizeCntr (V _)  S     =  decSizeCounter kvs	
				      opSizeCntr (Vp _) (V _) = return () 
				      --opSizeCntr _ _          = return ()				
				      -- TODO check witch changes are possible and witch arnt
				      -- TODO save sizecntr operationd on resize
					-- TODO what if T T


				      incSlotsCntr :: IO()
				      incSlotsCntr = incSlotsCounter kvs

-- TODO add reprobe count			
-- counter functions
------------------------------------------------------------------------------------------------------------------------
-- | Increments the counter of used slots in the array.
incSlotsCounter :: Kvs key value -> IO ()
incSlotsCounter kvs = do let counter = slotsCounter kvs
			 incrCounter_ 1 counter


incSizeCounter :: Kvs key value -> IO ()
incSizeCounter kvs = do let counter = sizeCounter kvs
			incrCounter_ 1 counter
-- TODO possibly parameter table

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


incReprobeCounter :: ReprobeCounter -> ReprobeCounter
incReprobeCounter cnt = cnt + 1

-- TODO possibly parameter table
--Exported functions

-- kvs functions
-----------------------------------------------------------------------------------------------------------------------------------------------------
--helper to acess first kvs
getHeadKvs :: ConcurrentHashTable key val -> IO(Kvs key val)
getHeadKvs table = do let kvsref= kvs table
		      readIORef kvsref


isHeadKvs :: ConcurrentHashTable key val -> Kvs key val -> IO Bool
isHeadKvs ht kv = do let headref = kvs ht  -- TODO Does this have to be in IO, ok
 		     hkv <- readIORef headref
		     return $ idKvs hkv kv  -- TODO ok, I wanted to make an pointer coparision, this would if it was a good idea require me to pass an IOref as parameter, witch would screw up the interface
--gets then new resizedtable, throws error if does not exist
getNextKvs :: Kvs key val -> IO(Kvs key val)
getNextKvs kv = do let kvsref =  newkvs kv  --throws error
		   nwkvs <- readIORef kvsref
		   return $ fromJust nwkvs 	  
-- TODO change if structure of kvs changes

--Is a resize in progress?
hasNextKvs :: Kvs key val -> IO Bool
hasNextKvs kv =  do let kvsref = newkvs kv
                    nwkvs <- readIORef kvsref
		    return $ isJust nwkvs

noKvs :: IO (IORef (Maybe (Kvs key val)))
noKvs = newIORef Nothing


--cas the newkvs field, returns the value in the newkvs field wether cas was succesfull or not  
casNextKvs :: Kvs key val -> Kvs key val -> IO (Kvs key val) -- TODO return the kvs wether
casNextKvs kvs nwkvs = do let kvsref = newkvs kvs
			  oldticket <- readForCAS kvsref
			  if isJust $ peekTicket oldticket then return $ fromJust $ peekTicket oldticket else do
					 (_, newticket) <- casIORef kvsref oldticket (Just nwkvs)
                                         return $ fromJust $ peekTicket newticket
 -- TODO rewrite the other cas stuff accordigly
-- TODO (Just IORef a) is a stupid construction because seting the IORef from Nothing to Just changes an immutable datastructure also you cant do an cas on the Maybe type, todo have some value of IORef that says nothing
-- TODO is this correctly

--TODO use oldheadkvs
casHeadKvs :: ConcurrentHashTable key val ->  Kvs key val -> Kvs key val -> IO ()
casHeadKvs ht oldheadkvs newkvs = do let kvsref = kvs ht
				     oldticket <- readForCAS kvsref
				     if  not (idKvs (peekTicket oldticket)  oldheadkvs) then return $ () else do -- TODO comparision of kvs ses
					(_,_) <- casIORef kvsref oldticket newkvs
					return ()
 -- TODO See if its still the old kvs in place, or actually would the correct thing not be
-- too get the ticket at the very beginning, tha is actually the only thing that makes sense

idKvs ::Kvs key val -> Kvs key val -> Bool
idKvs a b = getHeadKey a == getHeadKey b
		where getHeadKey k = key $ V.head $ slots k					 
-- Each kvs has its unique area of memory


--casHeadKvs2 :: ConcurrentHashTable key val -> IORef(Kvs key val) -> Kvs key val -> IO ()
--casHeadKvs2 = undefined


--casHeadKvs3 :: ConcurrentHashTable key val -> Ticket(Kvs key val) -> Kvs key val -> IO ()
--casHeadKvs3 = undefined

getLength :: Kvs key value -> Int
getLength = V.length . slots


casIncCopyIndex :: Kvs key val -> Size -> CopyIndex-> IO CopyIndex
casIncCopyIndex kvs oldlen minCopyWork = do let copyindexref = copyIndex kvs
					    ticket <- readForCAS copyindexref
					    oldindex <- return $ peekTicket ticket
					    if oldindex >= (shiftL oldlen 1) then return oldindex else do
						(success,newticket) <- casIORef copyindexref ticket (oldindex + minCopyWork)
						if success then return oldindex else casIncCopyIndex kvs oldlen minCopyWork -- TODO use the returned ticket instead of doing another readForCas
					
-------------------------------------------------------------------------------------------------------------

-- | Returns the number of key-value mappings in this map
size :: ConcurrentHashTable key val -> IO(Size)
size table = do let kvsref= kvs table
		kvs <- readIORef kvsref
		readSizeCounter kvs 


-- | Returns True if there are no key-value mappings
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


-- | Tests if the value is in the table.
--
--  __Attention:__ Unlike access by keys this is /computationaly very expensive,/ since it requires an traversal of the entire table.
--  If you do this a lot, you need a different datastructure. 
containsValue :: (Eq val) => ConcurrentHashTable key val -> val -> IO(Bool)
containsValue table val = do let kvsref = kvs table
			     kv <- readIORef kvsref
			     containsVal kv (V val)
-- TODO low priority
-- TODO adopt if changes to data representation
-- TODO adopt to resize


-- TODO search should break off once found
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
-- TODO adopt to resizing, (by recursivly calling for newkvs) anyway what about primed, I should read that up
-- TODO for this the linearisation point for inputing would be the cas on value even if the cas on key has not be done yet, actually its better to think about this for a while, maybe not export this function for a while
-- TODO no reason anyM should not be inlined

-- | puts the key-value mapping in the table, thus overwriting any pervious mapping of the key to an value
put :: (Eq val,Eq key, Hashable key) => 
       ConcurrentHashTable key val -> key 
       -> val 
       -> IO( Maybe val) -- ^ Just oldvalue if the key was mapped to an value perviously, Nothing if the key was not mapped to any value
put table key val = do old <- putIfMatch_T table key (V val) (Right NO_MATCH_OLD)
                       return $ unwrapValue old
-- | puts the value if there is no value matched to the key
putIfAbsent :: (Eq val,Eq key, Hashable key) => 
               ConcurrentHashTable key val 
	       -> key 
	       -> val 
	       -> IO( Maybe val) -- ^ 'Just' oldvalue if there was an mappig from key (/thus the put WAS NOT done/), 'Nothing' if there wasn't an
				 -- mapping (/thus the put WAS done/)
putIfAbsent table key val = do old <- putIfMatch_T table key (V val) (Left T) -- TODO is tombstone correct, what if there is a primed vaue 
			       return $ unwrapValue old

-- | Removes the key (and its corresponding value) from this map.
removeKey :: (Eq val, Eq key, Hashable key) =>
             ConcurrentHashTable key val -> key 
	     -> IO( Maybe val) -- ^ 'Just' oldvalue if removed, Nothing if key-value mapping was not in table
removeKey table key  = do old <- putIfMatch_T table key T (Right NO_MATCH_OLD)
		          return $ unwrapValue old

-- | Removes key if matched.
remove :: (Eq val, Eq key, Hashable key) =>
          ConcurrentHashTable key val -> key -> val
	  -> IO( Bool) -- ^ 'True' if key-value mapping removed, False key-value mapping was not in table
remove table key val = do old <- putIfMatch_T table key T (Left (V val))
                          return $  (unwrapValue old) == Just val  


-- | do a put if the key is already mapped to some value
replace :: (Eq val, Eq key, Hashable key) =>
	   ConcurrentHashTable key val -> key -> val 
	   -> IO( Maybe val) -- ^ Just old value if replaced, Nothing if not replaced
replace table key val = do old <- putIfMatch_T table key (V val) (Right MATCH_ANY)
                           return $ unwrapValue old

-- | do a put if the key is already mapped to the old value 
replaceTest :: (Eq val, Eq key, Hashable key) =>
               ConcurrentHashTable key val 
	       -> key -- ^ key
	       -> val -- ^ new value
	       -> val -- ^ old value 
	       -> IO(Bool) -- ^ True if replaced
replaceTest table key newval oldval= do old <- putIfMatch_T table key (V newval) (Left (V oldval))
                                        return $  (unwrapValue old) == Just oldval 
-- TODO Haddock comment gives actuall min_size
-- | Removes all of the mappings from this map, number of slots to min_size=2^3
--
-- may have concurrency bug
clear :: ConcurrentHashTable key val -> IO()
clear table = clearHint table min_size 

-- | Removes all of the mappings from this map, number of slots to next largest power of 2. See: 'newConcurrentHashTableHint'.
--
-- may have concurrency bug
clearHint :: ConcurrentHashTable key val -> Size -> IO()
clearHint table hint = do let size = normSize hint
                              kvsref = kvs table
		          szcntr <- newSizeCounter
                          kvs <- newKvs size szcntr
			  writeIORef kvsref kvs
-- TODO rewrite in case type of ConcurrentHashTable changes
-- TODO the java version uses a kvsCAS here
-- TODO write an concurrent testcase for this
-- TODO without cas a store load barrier might be in order, see discussion with ryan

-- | Returns the value to which the specified key is mapped.
get :: (Eq key, Hashable key) => 
       ConcurrentHashTable key val -> key ->  IO( Maybe val)
get table key = do topkvs <- getHeadKvs table            
		   result <- get_impl table topkvs (newKey key) 
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
				     timer <- newTimer
				     return $ ConcurrentHashTable kvsref timer
	where newTimer :: IO (IORef Time)
	      newTimer = do time <- getCurrentTime 
			    newIORef time -- TODO set actuall time -- TIMETODO                                   
-- TODO throw error if size <0

-- | Returns the next larger potency of 2
-- In case inputSize is potency of 2 : identity
normSize:: Size -> Size
normSize inputSize = max min_size (sizeHelp inputSize 1)
	where sizeHelp :: Size -> Size -> Size
	      sizeHelp input size = if (size >= input) || (size == max_size)  then size else sizeHelp input (shiftL size 1)	
--FIXME TODO guard for Integer overun meaning some maximum size has to be set


log2size :: Size -> SizeLog
log2size size = (intLog2 size) +1 -- Should be 

--size has to be power of 2
newKvs :: Size -> SizeCounter-> IO(Kvs key val)
newKvs size  counter = do let msk = getMask size
		          slts <- newSlots size
	                  sltcntr <- newSlotsCounter
			  kvsref <- noKvs
			  copyDone <- newIORef 0
			  copyIndex <- newCopyIndex
			  resizers <- newResizers
	                  return $ Kvs kvsref slts msk sltcntr counter copyDone copyIndex resizers
	where
		newSlots :: Size -> IO( Slots key val)
		newSlots size = V.replicateM size newSlot -- TODO
		newSlotsCounter :: IO(SlotsCounter)
		newSlotsCounter = newCounter 0
		newSlot	:: IO(Slot key val)
		newSlot = do keyref <- newIORef Kempty
			     valref <- newIORef T     -- TODO optimize somewhere, somewhat 
			     return $ Slot keyref valref 
		newCopyIndex = newIORef 0
		newResizers = newIORef 0
		

--Debug code -- TODO make inclusion conditional with preprocessor or something for DEBUG only
----------------------------------------------------------------------------------------------------------------------------------------------------

-- TODO shorten the output and make it more readable
class DebugShow a where
        debugShow :: a -> IO String
-- TODO Automatic indentation, ask on Stack Overflow about it.
--Debug print for the Hashtable
instance (Show k, Show v) => DebugShow (ConcurrentHashTable k v) where
        debugShow ht = do kvs <- getHeadKvs ht
                          str <- debugShow kvs 
                          return $  "ConcurrentHashtable:\n" ++ str

instance forall k v. (Show k, Show v) => DebugShow (Kvs k v) where
        debugShow kvs = debugshw 0 kvs
		where debugshw :: Int -> Kvs k v -> IO String
		      debugshw resizeCounter kvs = do str <- return $ "Kvs number " ++ (show resizeCounter) ++ " \n"
						      hsNextKvs <- hasNextKvs kvs
						      str <- return $ str ++  if not $ hsNextKvs then "Is newest Kvs.\n" else "Is older Kvs.\n" 
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
						      hsNextKvs <- hasNextKvs kvs
						      newKvsstr <-  if not $ hsNextKvs then return "END.\n" else 
								do nwKvs <- getNextKvs kvs
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
-- TODO number the slots
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

instance (Show k) => Show (Key k) where -- TODO keys get primed
	show (Key h key) = "Key: " ++ (show key)  -- TODO show FullHash 
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
              getNumber kvs = do hsNextKvs <- hasNextKvs kvs
				 if not $ hsNextKvs then return 0 else do nwkvs <- getNextKvs kvs
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
				  hsNextKvs <- hasNextKvs kvs
			          if not $ hsNextKvs then return $ lngth:[] else do newkvs <- getNextKvs kvs
									   	    lst <- getLengths newkvs
									            return $ lngth:lst 


getSlotsCounters ::ConcurrentHashTable k v-> IO [Int]
getSlotsCounters ht = mapOnKvs ht readSlotsCounter

countUsedSlots :: ConcurrentHashTable k v ->  IO [Int]
countUsedSlots ht = countSlotsWithPredicate ht (\s -> fmap not (isKEmptySlot s) ) -- TODO use point operator

countSlotsWithPredicate :: ConcurrentHashTable k v-> (Slot k v -> IO Bool) -> IO [Int]
countSlotsWithPredicate ht predicate = mapOnKvs ht (countSlots predicate)
	where countSlots ::(Slot k v -> IO Bool) -> (Kvs k v) -> IO Int
	      countSlots predicate kvs = do let slts = slots kvs
					    lst <- V.forM slts predicate
					    return $ V.foldl (\acc -> \bool -> if bool then acc + 1 else acc + 0) 0 lst  

mapOnKvs :: forall k v a. ConcurrentHashTable k v -> ((Kvs k v) -> IO a) -> IO [a]
mapOnKvs ht fun = do kvs <- getHeadKvs ht
		     mapOn kvs
	where mapOn :: Kvs k v -> IO [a]
              mapOn kvs = do a <- fun kvs
			     hsNextKvs <- hasNextKvs kvs
			     lst <- if not $ hsNextKvs then return [] else do newKvs <- getNextKvs kvs
								              mapOn newKvs
			     return $ a:lst


-- | Countes the number of Reprobes for a given key
-- does not change ht
getReprobeCount :: forall k v . (Hashable k, Eq k) =>  ConcurrentHashTable k v -> k -> IO Int
getReprobeCount ht key = do headkvs <- getHeadKvs ht
			    rpCount headkvs (newKey key)
	where rpCount :: Kvs k v -> Key k -> IO Int
	      rpCount kv key = do let msk = mask kv
                                      slts = slots kv
				  (_, rpcnt) <- getSlot  slts msk key
				  return rpcnt
-- TODO use this in an resize related unit test
-- work only on the headKvs

keyFullHashEqual :: (Hashable key) => key -> key -> Bool
keyFullHashEqual a b = (spreadHash $ hash a) == (spreadHash $ hash b)

keyIdxCollision :: forall key . (Hashable key) => Size -> key -> key -> Bool
keyIdxCollision sze a b = (getIdx a) == (getIdx b)
	where getFullHash :: key -> FullHash
	      getFullHash a = spreadHash $ hash a
	      getIdx a = maskHash (getMask sze) (getFullHash a)


--- Quick Check generator
------------------------------------------------------------------------------------------------------------------
instance (Eq val,Eq key, Hashable key) => QCA.Arbitrary (IO (ConcurrentHashTable key val)) where
	arbitrary = sized (\size -> htGen size size (Left size))
	shrink = (\ht -> [])


htGen :: forall key value .(Eq value,Eq key, Hashable key) => 
	Int -> Int -> Either Int (key -> Gen value) -> QCG.Gen (IO (ConcurrentHashTable key value))
htGen htsize keysize (Left valuesize) = undefined
--htGen htsize keysize (Left valuesize) = QCGU.promote $ do lst <- newConcurrentHashTable -- TODO In order to have this need a fúnction that takes an ht and returns IO Gen HT 	--			   	          ht
htGen htsize keysize (Right valuegen) = do gen <- h2
					   return gen -- TODO get h2 and add htsize times h3  
	where h1 :: QCG.Gen (IO (ConcurrentHashTable key value)) -> key -> value -> QCG.Gen (IO (ConcurrentHashTable key value)) 
	      h1 gen key val = fmap (\ioht -> ioht >>= (\ht -> (put ht key val) >> (return ht))) gen -- TODO question is there
	      h2 :: QCG.Gen (IO (ConcurrentHashTable key value))
	      h2 = return newConcurrentHashTable
	      h3 :: QCG.Gen (IO (ConcurrentHashTable key value)) -> Gen key -> Gen value -> QCG.Gen (IO (ConcurrentHashTable key value))
	      h3 gen genk genv = do k <- genk
				    v <- genv
				    h1 gen k v 
-- TODO now use monadic programing to feed h1 with Gen key and Gen values

-- we have a generator of Gen IO Concurrent hashtable and we fmap an put on it, probably requires some other monadic operation 
-- or what 
-- use of promote is correct but
-- First generator of a key-valuetor list 
-- IO of generator of an ht
-- then promote to generator of IO
-- then QuickCheck monadic


-- TODO parametrise this with custom keygen

-- TODO write a debug function telling the ht to arbitaritly resize

--todo generate arbitrary hashtables

--write an assertio for resize

-- TODO Assert each kvs does not contain the same key twice


-- TODO ticket api : key containing a fullhash, allows the user to cache hashes


-- TODO write comments on the interface

-- TODO how about an instance of arbitrary
-- |Set of Keys| = size , |List of Values| = size all genratet with size parameter, zip and put, actually not that compilcated



--code to write:
--copy one pair
--resize
--is headkvs
