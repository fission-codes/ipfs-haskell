module Network.IPFS.Error
  ( Error (..)
  , Linearization (..)
  ) where

import Servant.Server

import           Network.IPFS.Prelude
import           Network.IPFS.Types
import           Network.IPFS.ToServerError

import qualified Network.IPFS.Add.Error as Add
import qualified Network.IPFS.Get.Error as Get

data Error
  = AddErr Add.Error
  | GetErr Get.Error
  | LinearizationErr Linearization
  deriving ( Exception
           , Eq
           , Generic
           , Show
           , ToJSON
           )

instance ToServerError Error where
  toServerError = \case
    AddErr           addErr -> toServerError addErr
    GetErr           getErr -> toServerError getErr
    LinearizationErr linErr -> toServerError linErr

-- NOTE Will not stay as a newtype in the long term
newtype Linearization = NonLinear SparseTree
  deriving          ( Eq
                    , Generic
                    , Show
                    )
  deriving anyclass ( Exception
                    , ToJSON
                    )

instance Display Linearization where
  display (NonLinear sparseTree) = "Unable to linearize IPFS result: " <> display sparseTree

instance ToServerError Linearization where
  toServerError _ = err500 { errBody = "Unable to linearize IPFS result" }
