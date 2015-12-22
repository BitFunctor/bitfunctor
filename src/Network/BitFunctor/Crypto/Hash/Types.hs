module Network.BitFunctor.Crypto.Hash.Types ( HashAlgorithm (..)
                                            , Keccak_256
                                            , Hash (..)
                                            ) where

import Crypto.Hash.Algorithms (HashAlgorithm, Keccak_256)
import Crypto.Hash (Digest)
import Data.Aeson
import Data.ByteArray (convert)
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.Text.Encoding as TE


data HashAlgorithm a =>
     Hash a = Hash (Digest a)
              deriving (Eq, Ord, Show)

instance (HashAlgorithm a) => ToJSON (Hash a) where
  toJSON (Hash d) = String . TE.decodeUtf8 . B16.encode $ convert d
