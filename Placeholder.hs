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
	( ConcurrentHashTable,  insert,  delete, 
	)
	where

import GHC.IORef(IORef(IORef), readIORef)
import Data.Array.Unboxed(UArray)
import Data.Hashable(Hashable, hash)
-- import Data.Array.IArray((!))
import Data.Array.Unboxed((!))
import Data.Bits((.&.)) 


-- Kempty : empty, K : neverchanging key
data Key key = Kempty | K key 
-- T : empty, tombstone, Tp : tombstone primed, V : value, Vp : value primed
data Value value =  T | Tp |V value | Vp value 

data State k v =   State {
				key :: IORef (Key k)
				, value :: IORef (Value v)
				}



min_size_log = 3
min_size = 2 ^ min_size_log --must be power of 2, compiler should turn this into a constant


getMask:: Int -> Mask
getMask size_log = if size_log == 1 then 1 else (getMask ( size_log -1) )* 2 + 1 

type SlotsIndex = Int
type Mask = SlotsIndex


type Slots key val = UArray SlotsIndex (State key val) --TODO issue accessing the array generates a full copy, fix this latter 

data ConcurrentHashTable key val = ConcurrentHashTable {
		slots :: Slots key val --TODO, even if imutable it should be an IORef otherwise every handl to the hashmap will contain the whole array
		, mask :: Mask --TODO make this IORef, in any case since resizing there should be multipe masks
}


-- does not terminate if array is full, and key is not in it
-- TODO, use fitting hash function
--getSlot :: Hashable key => Slots key val -> Mask -> key -> State key val
--getSlot :: Hashable key => UArray SlotsIndex (State key val) -> Mask -> key -> State key val
getSlot slots mask key = slots ! ( hsh key mask) --TODO apply collision treatment
		where hsh :: (Hashable key) => key -> Mask -> SlotsIndex
		      hsh k m = (hash k)  .&. m--TODO apply mask
 --TODO use forall
-- new :: IO (ConcurrentHashTable a b)
-- new = return ConcurrentHashTable

insert :: ConcurrentHashTable key val -> key -> val -> IO ()
insert hash k v = return ()
{--
lookup :: Hashable key => ConcurrentHashTable key val -> key -> IO ( Maybe val)
lookup table k = do
		slot <- getSlot table k
		slotkey <- readIORef ( key slot)
		slotvalue <- readIORef (value slot)
		return $ getValue slotkey slotvalue
	where getSlot :: ConcurrentHashTable key val -> key -> State key val
	      getSlot tbl key = getSlot (slots tbl) (mask tbl)  
 
	      getValue :: Key key -> Value val -> Maybe val
	      getValue Kempty _ = Nothing
	      getValue (K _) v  = Just v 
--}
delete :: ConcurrentHashTable key val -> key -> IO ()
delete hash k =  return ()
	
