{-# LANGUAGE UndecidableInstances #-}

module Network.IPFS.Remote.Class
  ( MonadRemoteIPFS
  , ipfsAdd
  , ipfsCat
  , ipfsPin
  , ipfsUnpin
  ) where

import Network.IPFS.Prelude

import           Servant.Client
import qualified Network.HTTP.Client as HTTP
import qualified RIO.ByteString.Lazy as Lazy

import qualified Network.IPFS.Config as Config
import           Network.IPFS.Types as IPFS

import qualified Network.IPFS.Client as IPFS.Client
import qualified Network.IPFS.Client.Pin as Pin
import qualified Network.IPFS.File.Types      as File

class Monad m => MonadRemoteIPFS m where
  run       :: ClientM a -> m(Either ClientError a)
  ipfsAdd   :: Lazy.ByteString -> m (Either ClientError CID)
  ipfsCat   :: Text            -> m (Either ClientError File.Serialized)
  ipfsPin   :: Text            -> m (Either ClientError Pin.Response)
  ipfsUnpin :: Text -> Bool    -> m (Either ClientError Pin.Response)

instance 
  ( Has IPFS.URL cfg
  , Has HTTP.Manager cfg
  )
  => MonadRemoteIPFS (RIO cfg) where
    run query = do
      IPFS.URL url            <- Config.get
      manager :: HTTP.Manager <- Config.get
      url
        |> mkClientEnv manager
        |> runClientM query
        |> liftIO

    ipfsAdd raw             = run <| IPFS.Client.add raw
    ipfsCat cid             = run <| IPFS.Client.cat cid
    ipfsPin cid             = run <| IPFS.Client.pin cid
    ipfsUnpin cid recursive = run <| IPFS.Client.unpin cid recursive
