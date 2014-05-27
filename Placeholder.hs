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
	( ConcurrentHashTable,  insert, Data.HashTables.IO.Placeholder.lookup, delete, 
	)
	where

import GHC.IORef(IORef(IORef))
import Data.Array.Unboxed(UArray)

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


type SlotsIndex = Int

type Slots key val = UArray SlotsIndex (State key val)

data ConcurrentHashTable key val = ConcurrentHashTable {
		slots :: Slots key val}





-- new :: IO (ConcurrentHashTable a b)
-- new = return ConcurrentHashTable

insert :: ConcurrentHashTable key val -> key -> val -> IO ()
insert hash k v = return ()

lookup :: ConcurrentHashTable key val -> key -> IO ( Maybe val)
lookup table k = return Nothing
--	where getSlot :: ConcurrentHashTable key val -> key -> IO (State key val)

delete :: ConcurrentHashTable key val -> key -> IO ()
delete hash k =  return ()
	

