module Network.BitFunctor.Crypto.Hash.Types ( HashAlgorithm (..)
                                            , Keccak_256
                                            , Hash (..)
                                            ) where

import Crypto.Hash.Algorithms (HashAlgorithm, Keccak_256)
import Crypto.Hash (Digest)


data HashAlgorithm a =>
     Hash a = Hash (Digest a)
              deriving (Eq, Ord, Show)
