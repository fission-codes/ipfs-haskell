module Network.IPFS.Client.Streaming.Pin
  ( PinComplete
  , PinStatus (..)
  ) where

import           Servant.API

import           Network.IPFS.Prelude

import           Network.IPFS.CID.Types

type PinComplete
  =  "api"
  :> "v0"
  :> "pin"
  :> "add"
  :> QueryParam "arg"      CID
  :> QueryParam "progress" Bool
  :> StreamPost NewlineFraming JSON (SourceIO PinStatus)

data PinStatus = PinStatus
  { pins     :: [CID]
  , progress :: Maybe Natural
  }
  deriving (Eq, Show)

instance Display PinStatus where
  display status = displayShow status

instance FromJSON PinStatus where
  parseJSON = withObject "IPFS.PinStatus" \obj -> do
    pins     <- obj .:? "Pins"     .!= []
    progress <- obj .:? "Progress"
    return PinStatus {..}
