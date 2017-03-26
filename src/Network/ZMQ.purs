module Network.ZMQ where

import Control.Monad.Aff (Aff)
import Control.Monad.Error.Class (withResource)
import Data.ByteString (ByteString)
import Prelude

--------------------------------------------------------------------------------

foreign import data ZMQ :: Effect

--------------------------------------------------------------------------------

foreign import data Context :: Type

foreign import defaultContext :: ∀ eff. Aff (zmq :: ZMQ | eff) Context

--------------------------------------------------------------------------------

foreign import kind SocketType

data STProxy (st :: SocketType) = STProxy

class SocketType (st :: SocketType) where
  socketTypeName :: STProxy st -> String

foreign import data PUB    :: SocketType
foreign import data XPUB   :: SocketType
foreign import data SUB    :: SocketType
foreign import data XSUB   :: SocketType
foreign import data REQ    :: SocketType
foreign import data XREQ   :: SocketType
foreign import data REP    :: SocketType
foreign import data XREP   :: SocketType
foreign import data PUSH   :: SocketType
foreign import data PULL   :: SocketType
foreign import data DEALER :: SocketType
foreign import data ROUTER :: SocketType
foreign import data PAIR   :: SocketType
foreign import data STREAM :: SocketType

instance socketTypePUB    :: SocketType PUB    where socketTypeName _ = "pub"
instance socketTypeXPUB   :: SocketType XPUB   where socketTypeName _ = "xpub"
instance socketTypeSUB    :: SocketType SUB    where socketTypeName _ = "sub"
instance socketTypeXSUB   :: SocketType XSUB   where socketTypeName _ = "xsub"
instance socketTypeREQ    :: SocketType REQ    where socketTypeName _ = "req"
instance socketTypeXREQ   :: SocketType XREQ   where socketTypeName _ = "xreq"
instance socketTypeREP    :: SocketType REP    where socketTypeName _ = "rep"
instance socketTypeXREP   :: SocketType XREP   where socketTypeName _ = "xrep"
instance socketTypePUSH   :: SocketType PUSH   where socketTypeName _ = "push"
instance socketTypePULL   :: SocketType PULL   where socketTypeName _ = "pull"
instance socketTypeDEALER :: SocketType DEALER where socketTypeName _ = "dealer"
instance socketTypeROUTER :: SocketType ROUTER where socketTypeName _ = "router"
instance socketTypePAIR   :: SocketType PAIR   where socketTypeName _ = "pair"
instance socketTypeSTREAM :: SocketType STREAM where socketTypeName _ = "stream"

--------------------------------------------------------------------------------

foreign import data Socket :: SocketType -> Type

foreign import newSocket
  :: ∀ eff st
   . SocketType st
  => Context
  -> Aff (zmq :: ZMQ | eff) (Socket st)

foreign import closeSocket
  :: ∀ eff st
   . Socket st
  -> Aff (zmq :: ZMQ | eff) Unit

withSocket
  :: ∀ eff st a
   . SocketType st
  => Context
  -> (Socket st -> Aff (zmq :: ZMQ | eff) a)
  -> Aff (zmq :: ZMQ | eff) a
withSocket context = withResource (newSocket context) closeSocket

foreign import bindSocket
  :: ∀ eff st
   . Socket st
  -> String
  -> Aff (zmq :: ZMQ | eff) Unit

foreign import connectSocket
  :: ∀ eff st
   . Socket st
  -> String
  -> Aff (zmq :: ZMQ | eff) Unit

foreign import send
  :: ∀ eff st
   . Socket st
  -> Array ByteString
  -> Aff (zmq :: ZMQ | eff) Unit

foreign import receive
  :: ∀ eff st
   . Socket st
  -> Aff (zmq :: ZMQ | eff) (Array ByteString)
