module Network.IPFS.ServerError (ToServerError (..)) where

import           Servant.Server

class ToServerError err where
  toServerError :: err -> ServerError
