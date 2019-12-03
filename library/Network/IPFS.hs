module Network.IPFS
  ( MonadLocalIPFS
  , withIPFSProc
  , ipfsRun
  , MonadRemoteIPFS
  , runRemote
  , ipfsAdd
  , ipfsCat
  , ipfsPin
  , ipfsUnpin
  ) where

import Network.IPFS.Local.Class
import Network.IPFS.Remote.Class
