module Network.IPFS.Client
  ( API
  , add
  , cat
  , pin
  , run
  , unpin
  ) where

import qualified Network.HTTP.Client as HTTP
import qualified RIO.ByteString.Lazy as Lazy
import           Servant
import           Servant.Client

import           Network.Prelude
import qualified Network.Config as Config
import           Network.Internal.Orphanage.ByteString.Lazy ()

import qualified Network.File.Types      as File
import qualified Network.IPFS.Types      as IPFS
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

-- NOTE: May want to move these to streaming in the future
run
  :: ( MonadRIO         cfg m
     , Has IPFS.URL     cfg
     , Has HTTP.Manager cfg
     )
  => ClientM a
  -> m (Either ClientError a)
run query = do
  IPFS.URL url            <- Config.get
  manager :: HTTP.Manager <- Config.get

  url
    |> mkClientEnv manager
    |> runClientM query
    |> liftIO
