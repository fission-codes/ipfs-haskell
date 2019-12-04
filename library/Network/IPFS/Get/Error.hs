module Network.IPFS.Get.Error (Error (..)) where

import Servant.Server

import           Network.IPFS.Prelude
import qualified Network.IPFS.Internal.UTF8 as UTF8
import           Network.IPFS.Types
import           Network.IPFS.ToServerError

data Error
  = InvalidCID Text
  | TimedOut CID Natural
  | UnexpectedOutput Text
  | UnknownErr Text
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance Display Error where
  display = \case
    InvalidCID hash ->
      "Invalid CID: " <> display hash

    TimedOut (CID hash) sec ->
      mconcat
        [ "Unable to find CID "
        , display hash
        , " before the timeout of "
        , display sec
        , " seconds."
        ]

    UnexpectedOutput raw ->
      "Unexpected IPFS output: " <> display raw

    UnknownErr raw ->
      "Unknwon IPFS get error: " <> display raw

instance ToServerError Error where
  toServerError = \case
    InvalidCID txt          -> err422 { errBody = UTF8.textToLazyBS txt }
    UnexpectedOutput _ -> err500 { errBody = "Unexpected IPFS result" }
    UnknownErr _         -> err500 { errBody = "Unknown IPFS error" }
    (TimedOut (CID hash) _) ->
      ServerError { errHTTPCode     = 408
                  , errReasonPhrase = "Time out"
                  , errBody         = "IPFS timed out looking for " <> UTF8.textToLazyBS hash
                  , errHeaders      = []
                  }
