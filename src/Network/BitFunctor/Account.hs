module Network.BitFunctor.Account ( Account
                                  , AccountId
                                  , generate
                                  , mkAccount
                                  , pubKey
                                  , secKey
                                  , fromAccountId
                                  , toAccountId
                                  ) where

import Network.BitFunctor.Crypto.Types

import Crypto.PubKey.Ed25519 (toPublic, secretKey)
import Crypto.Error (onCryptoFailure, CryptoError)
import Crypto.Random.EntropyPool
import Data.ByteArray (ScrubbedBytes, Bytes, convert)
import qualified Data.ByteString.Base16 as B16 (encode, decode)

import Data.Aeson
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.Text.Encoding as TE


newtype AccountId = AccountId PublicKey
                    deriving (Show, Eq)

data Account = Account { pubKey :: PublicKey
                       , secKey :: Maybe SecretKey
                       } deriving Eq


instance Ord AccountId where
  compare (AccountId pk1) (AccountId pk2) = compare (toBytes pk1) (toBytes pk2)
    where toBytes :: PublicKey -> Bytes
          toBytes = convert

instance ToJSON AccountId where
  toJSON (AccountId pk) = toJSON pk

instance Show Account where
  show acc = "Account { pubKey = " ++ (show . B16.encode . convert $ pubKey acc) ++ ", secKey = <hidden> }"

mkAccount :: SecretKey -> Account
mkAccount sk = Account { pubKey = toPublic sk
                       , secKey = Just sk
                       }

generate :: IO (Either CryptoError Account)
generate = do
  pool <- createEntropyPool
  let secKeyLength = 32 -- cryptonite doesn't export that...
  entropy <- getEntropyFrom pool secKeyLength
  return $ onCryptoFailure Left
                           (Right . mkAccount)
                           (secretKey (entropy :: ScrubbedBytes))


fromAccountId :: AccountId -> Account
fromAccountId (AccountId pk) = Account { pubKey = pk, secKey = Nothing }

toAccountId :: Account -> AccountId
toAccountId = AccountId . pubKey
