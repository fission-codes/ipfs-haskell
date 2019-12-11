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

  Left (FailureResponse _a response) -> do
    let body = responseBody <| response
    let Just (IPFSErrorBody {message}) =  decode body
    let err = UnknownAddErr <| UTF8.textShow <| message
    trace "Hi mom" (return err)
    -- traceShow (display body)
    logError <| display <| traceShow err err

    return <| Left err

  Left err ->
    return <| Left <| UnknownAddErr <| UTF8.textShow err

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


-- type API = "ipfs" :> "add" :> Capture "cid" CID :> PUT '[JSON] ByteString

-- putAPI :: CID -> ClientM ByteString
-- putAPI = client (Proxy :: Proxy API)

-- env httper = ClientEnv     {
--     manager = httper
--     baseUrl = "localhost:5001"
--     cookieJar = Nothing
-- }

-- -- Run the request for realzies
-- httpManager <- HTTP.newManager HTTP.defaultManagerSettings

-- runClientM (env httpManager) putAPI