{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.BitFunctor.Block.Types where

import Network.BitFunctor.Transaction.Types (Transaction)
import Network.BitFunctor.Identifiable
import Network.BitFunctor.Account

import Data.ByteString
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import Data.Time.Clock (UTCTime)
import Network.BitFunctor.Crypto.Types
import Data.ByteArray (convert)
import Network.BitFunctor.Crypto.Hash (hash, Hash, Id)
import Network.BitFunctor.Common (UTCTimeAsPOSIXSeconds (..))

import Data.Binary as Bin (Binary(..), encode)
import Data.ByteString.Lazy (toStrict)
import Data.Word (Word8)


data Block = Block { previous     :: Hash Id
                   , timestamp    :: UTCTime
                   , transactions :: [Hash Id]
                   , baseTarget   :: Integer
                   , generator    :: AccountId
                   , signature    :: Signature
                   } deriving (Show, Eq, Generic)

instance Binary Block where
  put = undefined
  get = undefined


instance Identifiable Block where
  id = hash . toStrict . Bin.encode

-- instance FromJSON Block

instance ToJSON Block where
  toJSON b@(Block{}) = object [ "transactions" .= transactions b
                              , "timestamp"    .= timestamp b
                              , "baseTarget"   .= baseTarget b
                              , "generator"    .= generator b
                              , "signature"    .= signature b
                              ]

newtype BlockHash = Hash (Hash Id)
                    deriving (Show, Eq, Ord)
