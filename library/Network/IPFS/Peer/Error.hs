module Network.IPFS.Peer.Error (Error (..)) where

import Network.IPFS.Prelude
import Network.IPFS.Peer.Types

data Error
  = DecodeFailure String
  | CannotConnect Peer
  | UnknownErr Text
  deriving ( Exception
           , Eq
           , Generic
           , Show
           )

instance Display Error where
  display = \case
    DecodeFailure err  -> "Unable to decode: " <> displayShow err
    CannotConnect peer -> "Unable to connect to " <> display peer
    UnknownErr    msg  -> "Unknown IPFS peer list error: " <> display msg
