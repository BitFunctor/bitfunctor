{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.BitFunctor.Transaction.Types where

import Data.Time.Clock (UTCTime)
import Data.ByteArray (convert)
import GHC.Generics
import Data.Aeson
import Data.Text

import Network.BitFunctor.Crypto.Types
import qualified Network.BitFunctor.Theory.Types as Theory
import qualified Data.ByteString.Base16 as B16 (encode, decode)


data Transaction = Transaction { sender    :: PublicKey
                               , recipient :: PublicKey
                               , amount    :: Int
                               , fee       :: Int
                               , timestamp :: UTCTime
                               , payload   :: TheoryPayload
                               , signature :: Signature
                               } deriving (Show, Eq)

data TheoryPayload = TheoryPayload {
  uses     :: [(Theory.Kind, Text)],
  provides :: (Theory.Kind, Text),
  code     :: Theory.Code
} deriving (Eq, Show, Generic)


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
