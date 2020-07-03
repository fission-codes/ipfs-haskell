module Network.IPFS.Stat.Types (Stat(..)) where

import Network.IPFS.Prelude


data Stat = Stat 
  { blockSize      :: Natural
  , cumulativeSize :: Natural
  , dataSize       :: Natural
  , hash           :: Text
  , linksSize      :: Natural
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
