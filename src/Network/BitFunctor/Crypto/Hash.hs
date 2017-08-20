module Network.BitFunctor.Crypto.Hash ( HashAlgorithm (..)
                                      , Hash
                                      , Id
                                      , hash
                                      ) where

import Network.BitFunctor.Crypto.Hash.Types
import Data.ByteArray
import qualified Crypto.Hash as H (hash)


hash :: (ByteArrayAccess ba, HashAlgorithm a) => ba -> Hash a w
hash = Hash . H.hash
