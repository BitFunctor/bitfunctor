module Network.BitFunctor.Crypto.Hash.Types ( HashAlgorithm (..)
                                            , Keccak_256
                                            , Hash (..)
                                            , toString
                                            ) where

import Crypto.Hash.Algorithms (HashAlgorithm, Keccak_256)
import Crypto.Hash (Digest)
import Data.Aeson
import Data.ByteArray (convert)
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as DT (unpack)
import GHC.Generics

data HashAlgorithm a =>
     Hash a = Hash (Digest a)
              deriving (Eq, Ord, Show)

instance (HashAlgorithm a) => ToJSON (Hash a) where
  toJSON (Hash d) = String $ toText d

toString (Hash d) = DT.unpack $ toText d

toText d = TE.decodeUtf8 . B16.encode $ convert d
