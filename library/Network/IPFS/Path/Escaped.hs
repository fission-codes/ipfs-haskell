module Network.IPFS.Path.Escaped
  ( escape
  , module Network.IPFS.Path.Escaped.Types
  ) where

import           Network.IPFS.Prelude

import           Network.IPFS.Path.Escaped.Types

escape :: FilePath -> FilePath
escape path = show (fromString path :: Escaped)
