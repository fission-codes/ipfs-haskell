module Network.IPFS.Pin
  ( add
  , rm
  ) where

import           Network.IPFS.Prelude
import           Network.IPFS.Remote.Class
import qualified Network.IPFS.Internal.UTF8       as UTF8

import qualified Network.IPFS.Client.Pin     as Pin
import           Network.IPFS.Error          as IPFS.Error
import           Network.IPFS.Types          as IPFS

add ::
  ( MonadRIO        cfg m
  , MonadRemoteIPFS     m
  , HasLogFunc      cfg
  )
  => IPFS.CID
  -> m (Either IPFS.Error.Add CID)
add (CID hash) = ipfsPin hash >>= \case
  Right Pin.Response { cids } ->
    case cids of
      [cid] -> do
        logDebug <| "Pinned CID " <> display hash
        return <| Right cid

      _ ->
        logLeft <| UnexpectedOutput <| UTF8.textShow cids

  Left err ->
    logLeft err

-- | Unpin a CID
rm ::
  ( MonadRIO        cfg m
  , MonadRemoteIPFS     m
  , HasLogFunc      cfg
  )
  => IPFS.CID
  -> m (Either IPFS.Error.Add CID)
rm cid@(CID hash) = ipfsUnpin hash False >>= \case
  Right Pin.Response { cids } ->
    case cids of
      [cid'] -> do
        logDebug <| "Pinned CID " <> display hash
        return <| Right cid'

      _ ->
        logLeft <| UnexpectedOutput <| UTF8.textShow cids

  Left _ -> do
    logDebug <| "Cannot unpin CID " <> display hash <> " because it was not pinned"
    return <| Right cid

logLeft :: (MonadRIO cfg m, HasLogFunc cfg, Show a) => a -> m (Either IPFS.Error.Add b)
logLeft errStr = do
  let err = UnknownAddErr <| UTF8.textShow errStr
  logError <| display err
  return <| Left err
