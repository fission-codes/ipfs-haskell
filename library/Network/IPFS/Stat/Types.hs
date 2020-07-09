module Network.IPFS.Stat.Types (Stat(..)) where

import Network.IPFS.Prelude
import Network.IPFS.Bytes.Types


data Stat = Stat 
  { blockSize      :: Bytes
  , cumulativeSize :: Bytes
  , dataSize       :: Bytes
  , hash           :: Text
  , linksSize      :: Bytes
  , numLinks       :: Natural
  }

instance FromJSON Stat where
  parseJSON = withObject "Stat" \obj -> do
    blockSize      <- obj .: "BlockSize"
    cumulativeSize <- obj .: "CumulativeSize"
    dataSize       <- obj .: "DataSize"
    hash           <- obj .: "Hash"
    linksSize      <- obj .: "LinksSize"
    numLinks       <- obj .: "NumLinks"

    return Stat {..}
