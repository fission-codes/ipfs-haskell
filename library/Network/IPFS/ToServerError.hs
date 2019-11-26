module Network.IPFS.ToServerError (ToServerError (..)) where

import           Servant.Server

class ToServerError err where
  toServerError :: err -> ServerError
