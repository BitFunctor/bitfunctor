module Network.BitFunctor.Crypto.Key ( SecretKey
                                     , PublicKey
                                     , generateSecretKey
                                     , toPublic
                                     ) where

import Network.BitFunctor.Crypto.Key.Types

import Crypto.Random.Types
import qualified Crypto.PubKey.Ed25519 as C (toPublic)


generateSecretKey :: MonadRandom m => m SecretKey
generateSecretKey = undefined -- not yet in cryptonite-0.23: C.generateSecretKey

toPublic :: SecretKey -> PublicKey
toPublic = C.toPublic
