module Network.IPFS.Client.Stat (API) where

import Servant

import           Network.IPFS.Stat.Types
import qualified Network.IPFS.Client.Param as Param

type API = "stat"
        :> Param.CID
        :> Get '[JSON] Stat
