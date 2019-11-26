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
import qualified RIO.ByteString.Lazy as Lazy

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
  -- defaults
  ipfsAdd raw             = run <| IPFS.Client.add raw
  ipfsCat cid             = run <| IPFS.Client.cat cid
  ipfsPin cid             = run <| IPFS.Client.pin cid
  ipfsUnpin cid recursive = run <| IPFS.Client.unpin cid recursive
