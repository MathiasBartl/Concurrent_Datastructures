module Data.Atomics.Wrapper(
	casWrapper
	)
	where

import Data.Atomics
import GHC.IORef(IORef(IORef), readIORef, newIORef, writeIORef)

-- TODO write complete haddock documentation
-- | wrapper around 'casIORef'
casWrapper :: forall a . IORef a -> (a -> (Maybe a, b)) -> IO (Bool, Bool, a)
casWrapper ref fun = do ticket <- readForCAS ref
		        casW ref ticket fun
	where casW :: IORef a -> Ticket a -> (a -> (Maybe a,b)) -> IO (Bool,b, a)
	      casW ref ticket fun = do let oldvalue = peekTicket ticket
					   (newvalue, info) = fun oldvalue
					   if isNothing newvalue then return (False, info, oldvalue) else if undefined do (success, retticked) <- casIORef ref    (success, (\newval -> )

-- TODO use Maybe moadically to avoid the 

-- TODO as in casValueSlot
