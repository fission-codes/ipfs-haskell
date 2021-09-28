module Network.IPFS.Remote.Class
  ( MonadRemoteIPFS
  , runRemote
  , ipfsAdd
  , ipfsCat
  , ipfsStat
  , ipfsPin
  , ipfsUnpin
  ) where

import           Network.IPFS.Prelude

import qualified RIO.ByteString.Lazy       as Lazy
import           Servant.Client

import           Network.IPFS.Types        as IPFS

import qualified Network.IPFS.Client       as IPFS.Client
import qualified Network.IPFS.Client.Pin   as Pin
import qualified Network.IPFS.File.Types   as File

import           Network.IPFS.Remote.Error

class MonadIO m => MonadRemoteIPFS m where
  runRemote :: ClientM a       -> m (Either ClientError a)
  ipfsAdd   :: Lazy.ByteString -> m (Either ClientError CID)
  ipfsCat   :: CID             -> m (Either ClientError File.Serialized)
  ipfsStat  :: CID             -> m (Either StatError   Stat)
  ipfsPin   :: CID             -> m (Either ClientError Pin.Response)
  ipfsUnpin :: CID -> Bool     -> m (Either ClientError Pin.Response)

  -- defaults
  ipfsAdd   raw           = runRemote $ IPFS.Client.add raw
  ipfsCat   cid           = runRemote $ IPFS.Client.cat   cid
  ipfsPin   cid           = runRemote $ IPFS.Client.pin   cid
  ipfsUnpin cid recursive = runRemote $ IPFS.Client.unpin cid recursive

  ipfsStat cid =
    runRemote (IPFS.Client.stat  cid) >>= \case
      Left  clientErr       -> return . Left $ WebError  clientErr
      Right (Left sizeErr)  -> return . Left $ SizeError sizeErr
      Right (Right payload) -> return $ Right payload
