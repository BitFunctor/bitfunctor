{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.BitFunctor.Transaction.Types where

import Data.Time.Clock (UTCTime)
import Data.ByteArray (convert)
import GHC.Generics
import Data.Aeson
import Data.Text

import Network.BitFunctor.Crypto.Types
import Network.BitFunctor.Crypto.Hash (hash)
import qualified Network.BitFunctor.Theory.Types as Theory
import Network.BitFunctor.Identifiable

import qualified Data.ByteString.Base16 as B16 (encode, decode)

import Data.Binary as Binary (Binary(..), encode)
import Data.ByteString.Lazy (toStrict)



data Transaction = Transaction { sender    :: PublicKey
                               , recipient :: PublicKey
                               , amount    :: Int
                               , fee       :: Int
                               , timestamp :: UTCTime
                               , payload   :: TheoryPayload
                               , signature :: Signature
                               } deriving (Show, Eq, Generic)

data TheoryPayload = TheoryPayload {
  uses     :: [(Theory.Kind, Text)],
  provides :: (Theory.Kind, Text),
  code     :: Theory.Code
} deriving (Eq, Show, Generic)


instance Identifiable Transaction where
  id = hash . toStrict . Binary.encode

instance Binary Transaction where
  put = undefined
  get = undefined

instance Binary TheoryPayload

-- instance FromJSON Transaction

instance ToJSON Transaction where
  toJSON tx@(Transaction{}) = object [ "sender"    .= show (B16.encode $ convert $ sender tx)
                                     , "recipient" .= show (B16.encode $ convert $ recipient tx)
                                     , "amount"    .= amount tx
                                     , "fee"       .= fee tx
                                     , "timestamp" .= timestamp tx
                                     , "payload"   .= payload tx
                                     , "signature" .= show (B16.encode $ convert $ signature tx)
                                     ]


instance FromJSON TheoryPayload

instance ToJSON TheoryPayload where
  toEncoding = genericToEncoding defaultOptions