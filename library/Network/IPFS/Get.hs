module Network.IPFS.Get
  ( getFile
  , getFileOrDirectory
  ) where

import           Network.IPFS.Prelude
import           Network.IPFS.Local.Class
import           Network.IPFS.Internal.Process
import qualified Network.IPFS.Internal.UTF8       as UTF8

import           Data.ByteString.Lazy.Char8 as CL
import qualified RIO.ByteString.Lazy as Lazy
import qualified RIO.Text as Text

import qualified Network.IPFS.Config              as Config
import qualified Network.IPFS.File.Types          as File
import           Network.IPFS.Error          as IPFS.Error
import           Network.IPFS.Types          as IPFS

getFileOrDirectory ::
  MonadLocalIPFS m
  => IPFS.CID
  -> m (Either IPFS.Error.Get CL.ByteString)
-- getFileOrDirectory (IPFS.CID hash) = IPFS.Proc.run ["get", Text.unpack hash] "" >>= \case
getFileOrDirectory (IPFS.CID hash) = ipfsRun ["get", Text.unpack hash] "" >>= \case
  (ExitSuccess, contents, _) ->
    return <| Right contents

  (ExitFailure _, _, stdErr) ->
    return . Left . UnknownGetErr <| UTF8.textShow stdErr

getFile ::
  ( RIOProc           cfg m
  , MonadLocalIPFS        m
  , Has IPFS.Timeout cfg
  )
  => IPFS.CID
  -> m (Either IPFS.Error.Get File.Serialized)
getFile cid@(IPFS.CID hash) = ipfsRun ["cat"] (UTF8.textToLazyBS hash) >>= \case
  (ExitSuccess, contents, _) ->
    return . Right <| File.Serialized contents

  (ExitFailure _, _, stdErr)
    | Lazy.isPrefixOf "Error: invalid 'ipfs ref' path" stdErr ->
        return . Left <| InvalidCID hash

    | Lazy.isSuffixOf "context deadline exceeded" stdErr -> do
        Timeout seconds <- Config.get
        return . Left <| TimedOut cid seconds

    | otherwise ->
        return . Left . UnknownGetErr <| UTF8.textShow stdErr
