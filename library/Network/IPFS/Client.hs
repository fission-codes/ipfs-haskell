module Network.IPFS.Client
  ( API
  , add
  , cat
  , stat
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
import           Network.IPFS.Stat.Types

import qualified Network.IPFS.Client.Add    as Add
import qualified Network.IPFS.Client.Cat    as Cat
import qualified Network.IPFS.Client.Pin    as Pin
import qualified Network.IPFS.Client.Stat as Stat

type API
  = "api"
  :> "v0"
  :> V0API

type V0API = "add"  :> Add.API
        :<|> "cat"  :> Cat.API
        :<|> "stat" :> Stat.API
        :<|> "pin"  :> Pin.API

add   :: Lazy.ByteString -> ClientM CID
cat   :: Text            -> ClientM File.Serialized
stat  :: Text            -> ClientM Stat
pin   :: Text            -> ClientM Pin.Response
unpin :: Text -> Bool    -> ClientM Pin.Response

add :<|> cat
    :<|> stat 
    :<|> pin
    :<|> unpin 
    = client <| Proxy @API
