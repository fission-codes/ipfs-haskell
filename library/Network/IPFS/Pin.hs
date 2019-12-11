module Network.IPFS.Pin
  ( add
  , rm
  ) where

import           Network.IPFS.Prelude
import           Network.IPFS.Remote.Class
import qualified Network.IPFS.Internal.UTF8       as UTF8

import qualified Network.IPFS.Client.Pin     as Pin
import           Network.IPFS.Add.Error      as IPFS.Add
import           Network.IPFS.Types          as IPFS

add ::
  ( MonadRemoteIPFS m
  , MonadLogger     m
  )
  => IPFS.CID
  -> m (Either IPFS.Add.Error CID)
add cid = ipfsPin cid >>= \case
  Right Pin.Response { cids } ->
    case cids of
      [cid'] -> do
        logDebug <| "Pinned CID " <> display cid'
        return <| Right cid'

      _ ->
        logLeft <| UnexpectedOutput <| UTF8.textShow cids

  Left err ->
    logLeft err

-- | Unpin a CID
rm ::
  ( MonadRemoteIPFS  m
  , MonadLogger      m
  )
  => IPFS.CID
  -> m (Either IPFS.Add.Error CID)
rm cid = ipfsUnpin cid False >>= \case
  Right Pin.Response { cids } ->
    case cids of
      [cid'] -> do
        logDebug <| "Pinned CID " <> display cid'
        return <| Right cid'

      _ ->
        logLeft <| UnexpectedOutput <| UTF8.textShow cids

  Left _ -> do
    logDebug <| "Cannot unpin CID " <> display cid <> " because it was not pinned"
    return <| Right cid

logLeft ::
  ( MonadLogger m
  , Show a
  )
  => a
  -> m (Either IPFS.Add.Error b)
logLeft errStr = do
  let err = UnknownAddErr <| UTF8.textShow errStr
  logError <| display err
  return <| Left err
