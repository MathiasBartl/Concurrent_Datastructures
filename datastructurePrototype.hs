import Data.Hashable
import GHC.IORef
import Data.Maybe


type FullHash = Int
data State k v =   State {
				keey :: IORef k
				, fullhash :: !(Maybe FullHash)
				, value :: IORef v
				, flags :: Int
				}

data Key k = Kempty | Key { fullHash :: !FullHash
		 , key :: !k
		 } 

newKey :: Hashable k => k -> Key k
newKey k = Key (hash k) k


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
