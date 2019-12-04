module Network.IPFS.Peer
  ( all
  , rawList
  , connect
  , getExternalAddress
  ) where

import qualified RIO.Text            as Text
import qualified RIO.List            as List

import qualified Net.IPv4 as IPv4
import           Text.Regex

import           Network.IPFS.Prelude hiding (all)
import qualified Network.IPFS.Internal.UTF8       as UTF8

import qualified Network.IPFS.Types         as IPFS
import qualified Network.IPFS.Process.Error as Process
import           Network.IPFS.Local.Class   as IPFS
import           Network.IPFS.Peer.Error    as IPFS.Peer
import           Network.IPFS.Peer.Types
import           Network.IPFS.Info.Types

all ::
  MonadLocalIPFS m
  => m (Either IPFS.Peer.Error [IPFS.Peer])
all = rawList <&> \case
  Right raw -> case UTF8.encode raw of
    Left  _    -> Left  <| DecodeFailure <| show raw
    Right text -> Right <| IPFS.Peer <$> Text.lines text
  Left err -> Left . UnknownErr <| UTF8.textShow err

rawList ::
  MonadLocalIPFS m
  => m (Either Process.Error Process.RawMessage)
rawList = IPFS.runLocal ["bootstrap", "list"] ""

connect ::
  MonadLocalIPFS m
  => Peer
  -> m (Either IPFS.Peer.Error ())
connect peer@(Peer peerID) = IPFS.runLocal ["swarm", "connect"] (UTF8.textToLazyBS peerID) >>= pure . \case
  Left _ -> Left <| CannotConnect peer
  Right _ -> Right ()

peerAddressRe :: Regex
peerAddressRe = mkRegex "^/ip[46]/([a-zA-Z0-9.:]*)/"

-- | Retrieve just the ip address from a peer address
extractIPfromPeerAddress :: String -> Maybe String
extractIPfromPeerAddress peer = matchRegex peerAddressRe peer >>= List.headMaybe

-- | True if a given peer address is externally accessable
isExternalIPv4 :: Text -> Bool
isExternalIPv4 ip = maybe False not isReserved
  where
    isReserved :: Maybe Bool
    isReserved = do
      ipAddress  <- extractIPfromPeerAddress <| Text.unpack ip
      normalized <- IPv4.decode <| Text.pack ipAddress
      return <| IPv4.reserved normalized

-- | Filter a list of peers to include only the externally accessable addresses
filterExternalPeers :: [Peer] -> [Peer]
filterExternalPeers = filter (isExternalIPv4 . peer)

-- | Get all external ipfs peer addresses
getExternalAddress ::
  MonadLocalIPFS m
  => m (Either IPFS.Peer.Error [Peer])
getExternalAddress = IPFS.runLocal ["id"] "" >>= \case
  Left err -> return <| Left <| UnknownErr <| UTF8.textShow err
  Right raw ->
    raw
      |> decode
      |> maybe [] addresses
      |> Right . filterExternalPeers
      |> pure
