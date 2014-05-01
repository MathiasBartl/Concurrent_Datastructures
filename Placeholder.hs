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
	( ConcurrentHashTable, new, insert, Data.HashTables.IO.Placeholder.lookup, delete, 
	)
	where

data ConcurrentHashTable key val = ConcurrentHashTable {}

new :: IO (ConcurrentHashTable a b)
new = return ConcurrentHashTable

insert :: ConcurrentHashTable key val -> key -> val -> IO ()
insert hash k v = return ()

lookup :: ConcurrentHashTable key val -> key -> IO ( Maybe val)
lookup hash k = return Nothing

delete :: ConcurrentHashTable key val -> key -> IO ()
delete hash k =  return ()
