{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Internal.Orphanage.ByteString.Lazy () where

import qualified RIO.ByteString.Lazy as Lazy
import           Servant

import           Network.Prelude

instance MimeRender PlainText Lazy.ByteString where
  mimeRender _proxy = identity

instance FromJSON ByteString where
  parseJSON = withText "ByteString" (pure . encodeUtf8)
