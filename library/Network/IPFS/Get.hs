module Network.IPFS.Get
  ( getFile
  , getFileOrDirectory
  ) where

import           Network.IPFS.Prelude
import           Network.IPFS.Local.Class
import qualified Network.IPFS.Internal.UTF8       as UTF8

import           Data.ByteString.Lazy.Char8 as CL
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text as Text

import qualified Network.IPFS.File.Types          as File
import           Network.IPFS.Timeout.Types
import           Network.IPFS.Error          as IPFS.Error
import           Network.IPFS.Types          as IPFS

getFileOrDirectory ::
  MonadLocalIPFS m
  => IPFS.CID
  -> m (Either IPFS.Error.Get CL.ByteString)
getFileOrDirectory (IPFS.CID hash) = ipfsRun ["get", Text.unpack hash] "" >>= \case
  (ExitSuccess, contents, _) ->
    return <| Right contents

  (ExitFailure _, _, stdErr) ->
    return . Left . UnknownGetErr <| UTF8.textShow stdErr

getFile ::
  MonadLocalIPFS m
  => IPFS.CID
  -> m (Either IPFS.Error.Get File.Serialized)
getFile cid@(IPFS.CID hash) = ipfsRun ["cat"] (UTF8.textToLazyBS hash) >>= \case
  (ExitSuccess, contents, _) ->
    return . Right <| File.Serialized contents

  (ExitFailure _, _, stdErr)
    | Lazy.isPrefixOf "Error: invalid 'ipfs ref' path" stdErr ->
        return . Left <| InvalidCID hash

    | Lazy.isSuffixOf "context deadline exceeded" stdErr -> do
        timeout <- getTimeout
        return . Left <| TimedOut cid <| getSeconds timeout

    | otherwise ->
        return . Left . UnknownGetErr <| UTF8.textShow stdErr
