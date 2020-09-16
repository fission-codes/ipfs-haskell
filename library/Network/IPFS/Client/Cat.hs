module Network.IPFS.Client.Cat (API) where

import Servant.API

import qualified Network.IPFS.Client.Param as Param
import qualified Network.IPFS.File.Types   as File

import Network.IPFS.MIME.RawPlainText.Types

-- IPFS v0.5 disallows GET requests
-- https://docs.ipfs.io/recent-releases/go-ipfs-0-5/#breaking-changes-upgrade-notes
type API = Param.CID
        :> Post '[PlainText, RawPlainText] File.Serialized
