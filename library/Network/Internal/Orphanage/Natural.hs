{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Network.Internal.Orphanage.Natural () where

import System.Envy

import Network.Prelude

instance Display Natural where
  display nat = display (fromIntegral nat :: Integer)

instance Var Natural where
  fromVar = readMaybe
