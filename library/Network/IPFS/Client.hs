module Network.IPFS.Client
  ( API
  , add
  , cat
  , pin
  , unpin
  ) where

import qualified RIO.ByteString.Lazy as Lazy
import           Servant
import           Servant.Client

import           Network.IPFS.Prelude
import           Network.IPFS.Internal.Orphanage.ByteString.Lazy ()

import qualified Network.IPFS.File.Types      as File
import           Network.IPFS.CID.Types

import qualified Network.IPFS.Client.Add as Add
import qualified Network.IPFS.Client.Cat as Cat
import qualified Network.IPFS.Client.Pin as Pin

type API
  = "api"
  :> "v0"
  :> V0API

type V0API = "add" :> Add.API
        :<|> "cat" :> Cat.API
        :<|> "pin" :> Pin.API

add   :: Lazy.ByteString -> ClientM CID
cat   :: Text            -> ClientM File.Serialized
pin   :: Text            -> ClientM Pin.Response
unpin :: Text -> Bool    -> ClientM Pin.Response

add :<|> cat
    :<|> pin
    :<|> unpin = client <| Proxy @API
