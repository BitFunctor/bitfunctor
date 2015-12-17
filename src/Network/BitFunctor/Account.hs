module Network.BitFunctor.Account ( Account (..)
                                  , generate
                                  ) where

import Network.BitFunctor.Crypto.Types

import Crypto.PubKey.Ed25519 (toPublic, secretKey)
import Crypto.Error (onCryptoFailure, CryptoError)
import Crypto.Random.EntropyPool
import Data.ByteArray (ScrubbedBytes)


data Account = Account { pubKey :: PublicKey
                       , secKey :: Maybe SecretKey
                       } deriving Eq


generate :: IO (Either CryptoError Account)
generate = do
  pool <- createEntropyPool
  let secKeyLength = 32 -- cryptonite doesn't export that...
  entropy <- getEntropyFrom pool secKeyLength
  return $ onCryptoFailure Left
                           (\sk -> Right $ Account { pubKey = toPublic sk
                                                   , secKey = Just sk })
                           (secretKey (entropy :: ScrubbedBytes))
