-- | A custom @Prelude@-like module for this project
module Network.IPFS.Prelude
  ( module Control.Lens
  , module Data.Aeson
  , module Data.Has
  , module Data.Maybe
  , module Network.IPFS.Internal.Constraint
  , module Flow
  , module RIO
  , module RIO.Process
  , module RIO.Time
  , headMaybe
  , identity
  , intercalate
  -- , putText
  -- , putTextLn
  -- , textShow
  ) where

import Control.Lens                ((%~), (.~), (?~), (^?))
import Data.Aeson
import Data.Has
import Data.Maybe
import Network.IPFS.Internal.Constraint

-- import Network.IPFS.Internal.UTF8       (putText, putTextLn, textShow)
import Flow
import RIO                         hiding (Handler, id, timeout, ($), (&))
import RIO.List                    (headMaybe, intercalate)
import RIO.Process
import RIO.Time

identity :: a -> a
identity a = a
