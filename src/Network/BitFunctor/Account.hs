module Network.BitFunctor.Account ( Account (..)
                                  , AccountId
                                  , generate
                                  , fromAccountId
                                  , toAccountId
                                  ) where

import Network.BitFunctor.Crypto.Types

import Crypto.PubKey.Ed25519 (toPublic, secretKey)
import Crypto.Error (onCryptoFailure, CryptoError)
import Crypto.Random.EntropyPool
import Data.ByteArray (ScrubbedBytes, convert)

import Data.Aeson
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.Text.Encoding as TE


newtype AccountId = AccountId PublicKey
                    deriving (Show, Eq)

instance ToJSON AccountId where
  toJSON (AccountId pk) = toJSON pk


data Account = Account { pubKey :: PublicKey
                       , secKey :: Maybe SecretKey
                       } deriving Eq


generate :: IO (Either CryptoError Account)
generate = do
  pool <- createEntropyPool
  let secKeyLength = 32 -- cryptonite doesn't export that...
  entropy <- getEntropyFrom pool secKeyLength
  return $ onCryptoFailure Left
                           (\sk -> Right Account { pubKey = toPublic sk
                                                 , secKey = Just sk })
                           (secretKey (entropy :: ScrubbedBytes))

fromAccountId :: AccountId -> Account
fromAccountId (AccountId pk) = Account { pubKey = pk, secKey = Nothing }

toAccountId :: Account -> AccountId
toAccountId = AccountId . pubKey
