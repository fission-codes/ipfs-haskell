module Network.IPFS.Process.Error
  ( Error (..)
  , RawMessage
  ) where

import Network.IPFS.Prelude
import Data.ByteString.Lazy.Char8 as CL

data Error
  = Timeout Natural
  | UnknownErr RawMessage
  deriving ( Exception
           , Eq
           , Generic
           , Show
           )

instance Display Error where
  display = \case
    Timeout _ -> "IPFS timed out"
    UnknownErr raw -> displayShow raw

type RawMessage = CL.ByteString
