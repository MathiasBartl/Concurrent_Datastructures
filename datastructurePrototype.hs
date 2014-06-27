type FullHash = Int
data State k v =   State {
				key :: IORef k
				, hash :: !(Just FullHash)
				, value :: IORef v
				, flags
				}
