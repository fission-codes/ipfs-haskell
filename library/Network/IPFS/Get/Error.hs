module Network.IPFS.Get.Error (Error (..)) where

import           Network.IPFS.Prelude
import           Network.IPFS.Types

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
      "Unknown IPFS get error: " <> display raw
