module Network.IPFS.Add.Error (Error (..)) where

import           Network.IPFS.Prelude
import qualified Network.IPFS.Get.Error as Get

data Error
  = InvalidFile
  | UnexpectedOutput Text
  | RecursiveAddErr Get.Error
  | IPFSDaemonErr Text
  | UnknownAddErr Text
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance Display Error where
  display = \case
    InvalidFile          -> "Invalid file"
    UnexpectedOutput txt -> "Unexpected IPFS output: " <> display txt
    RecursiveAddErr  err -> "Error while adding directory" <> display err
    IPFSDaemonErr    txt -> "IPFS Daemon error:" <> display txt
    UnknownAddErr    txt -> "Unknown IPFS add error: " <> display txt
