-- | A custom @Prelude@-like module for this project
module Network.IPFS.Prelude
  ( module Control.Lens
  , module Data.Aeson
  , module Network.IPFS.Internal.Constraint
  , module Flow
  , module RIO
  , module RIO.Process
  , identity
  ) where

import Control.Lens                ((%~), (.~), (?~), (^?))
import Data.Aeson
import Network.IPFS.Internal.Constraint

import Flow
import RIO                         hiding (Handler, id, timeout, ($), (&))
import RIO.Process

identity :: a -> a
identity a = a
