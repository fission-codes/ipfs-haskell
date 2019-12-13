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
import           Servant.Client.Core


data IPFSErrorBody = IPFSErrorBody {message :: String}
instance FromJSON IPFSErrorBody where
  parseJSON = withObject "IPFSErrorBody" \obj -> do
    message    <- obj .: "Message"
    return <| IPFSErrorBody {..}

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

  Left err -> do
    newError <- formatIpfsAddError err
    return <| Left <| newError

formatIpfsAddError ::
  ( MonadRIO        cfg m
  , MonadRemoteIPFS     m
  , HasLogFunc      cfg
  )
  => ClientError
  -> m (Error)
formatIpfsAddError err = do
  logError <| displayShow err
  let newError = case err of
        (FailureResponse _ response) -> do
          response
          |> responseBody
          |> decode
          |> \case
            Just (IPFSErrorBody {message}) ->
              KnownAddErr <| UTF8.textShow message

            _ ->
              UnknownAddErr <| UTF8.textShow err
        _ ->
          UnknownAddErr <| UTF8.textShow err

  return newError

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
  ( MonadRIO cfg m
  , HasLogFunc cfg
  )
  => IPFS.Add.Error
  -> m (Either IPFS.Add.Error b)
logLeft errStr = do
  let err = UnknownAddErr <| UTF8.textShow errStr
  logError <| display err
  return <| Left err
