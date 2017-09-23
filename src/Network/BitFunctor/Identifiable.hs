module Network.BitFunctor.Identifiable where

import Network.BitFunctor.Crypto.Hash (Hash, Id, hash)
import Data.Binary (Binary (..), encode)
import Data.ByteString.Lazy (toStrict)

class Identifiable a where
  id :: a -> Hash Id a

newtype ByBinary a = ByBinary a

instance Binary a => Identifiable (ByBinary a) where
  id (ByBinary x) = hash . toStrict . encode $ x
