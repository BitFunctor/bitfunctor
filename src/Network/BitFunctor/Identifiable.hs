module Network.BitFunctor.Identifiable where

import Network.BitFunctor.Crypto.Hash (Hash, Id)

class Identifiable a where
  id :: a -> Hash Id
