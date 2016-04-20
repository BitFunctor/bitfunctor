{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.BitFunctor.Transaction.Types ( Transaction (..)
                                            , TxInput (..)
                                            , TxInputType (..)
                                            , TxOutput (..)
                                            , TransactionHash
                                            , TheoryPayload (..)
                                            ) where

import Data.Time.Clock (UTCTime)
import Data.ByteArray (convert)
import GHC.Generics
import Data.Aeson
import Data.Text

import Network.BitFunctor.Account
import Network.BitFunctor.Token
import Network.BitFunctor.Crypto.Types
import Network.BitFunctor.Crypto.Hash (hash, Hash, Id)
import qualified Network.BitFunctor.Theory.Types as Theory
import Network.BitFunctor.Identifiable

import qualified Data.ByteString.Base16 as B16 (encode, decode)

import Data.Binary as Binary (Binary(..), encode)
import Data.ByteString.Lazy (toStrict)


data TxInput = TxInput { sender    :: AccountId
                       , inputType :: TxInputType
                       } deriving (Show, Eq, Generic)

data TxInputType = Value        { amount      :: BTF }
                 | Option       { option      :: CBTF }
                 | OptionCreate { payload     :: TheoryPayload }
                 | OptionBurn   { claimOption :: CBTF
                                , claimAmount :: BTF
                                }
                 deriving (Show, Eq, Generic)

data TxOutput = TxOutput { recipient :: AccountId }
              deriving (Show, Eq, Generic)

data Transaction = Transaction { input     :: TxInput
                               , output    :: TxOutput
                               , fee       :: BTF
                               , timestamp :: UTCTime
                               , signature :: Signature
                               } deriving (Show, Eq, Generic)

-- Code commented to be compiled
data TheoryPayload = TheoryPayload {
  uses :: [Hash Id]
 -- , code :: Theory.Code
} deriving (Eq, Show, Generic)


instance Identifiable Transaction where
  id = hash . toStrict . Binary.encode

instance Binary Transaction where
  put = undefined
  get = undefined

--instance Binary TheoryPayload


--instance ToJSON TxInputType
--instance ToJSON TxInput
--instance ToJSON TxOutput

-- instance FromJSON Transaction
--instance ToJSON Transaction
 --where
 -- toJSON tx@(Transaction{}) = object [ "sender"    .= sender tx
 --                                    , "recipient" .= recipient tx
 --                                    , "amount"    .= value (amount tx)
 --                                    , "fee"       .= value (fee tx)
 --                                    , "timestamp" .= timestamp tx
 --                                    , "payload"   .= payload tx
 --                                    , "signature" .= signature tx
 --                                    ]


--instance FromJSON TheoryPayload

--instance ToJSON TheoryPayload
 --where
 -- toEncoding = genericToEncoding defaultOptions

newtype TransactionHash = Hash (Hash Id)
                          deriving (Show, Eq, Ord)
