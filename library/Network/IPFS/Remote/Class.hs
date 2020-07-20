module Network.IPFS.Remote.Class
  ( MonadRemoteIPFS
  , runRemote
  , ipfsAdd
  , ipfsCat
  , ipfsStat
  , ipfsPin
  , ipfsUnpin
  ) where

import Network.IPFS.Prelude

import           Servant.Client
import qualified RIO.ByteString.Lazy as Lazy

import           Network.IPFS.Types as IPFS

import qualified Network.IPFS.Client as IPFS.Client
import qualified Network.IPFS.Client.Pin as Pin
import qualified Network.IPFS.File.Types      as File


class MonadIO m => MonadRemoteIPFS m where
  runRemote :: ClientM a       -> m (Either ClientError a)
  ipfsAdd   :: Lazy.ByteString -> m (Either ClientError CID)
  ipfsCat   :: CID             -> m (Either ClientError File.Serialized)
  ipfsStat  :: CID             -> m (Either ClientError Stat)
  ipfsPin   :: CID             -> m (Either ClientError Pin.Response)
  ipfsUnpin :: CID -> Bool     -> m (Either ClientError Pin.Response)

  -- defaults
  ipfsAdd raw                   = runRemote <| IPFS.Client.add raw
  ipfsCat (CID cid)             = runRemote <| IPFS.Client.cat cid
  ipfsStat (CID cid)            = runRemote <| IPFS.Client.stat cid
  ipfsPin (CID cid)             = runRemote <| IPFS.Client.pin cid
  ipfsUnpin (CID cid) recursive = runRemote <| IPFS.Client.unpin cid recursive
