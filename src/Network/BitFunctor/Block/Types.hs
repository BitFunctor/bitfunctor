{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.BitFunctor.Block.Types where

import Network.BitFunctor.Transaction.Types (Transaction)
import Network.BitFunctor.Identifiable

import Data.ByteString
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import Data.Time.Clock (UTCTime)
import Network.BitFunctor.Crypto.Types
import Data.ByteArray (convert)
import Network.BitFunctor.Crypto.Hash (hash, Hash, Id)

import Data.Binary as Bin (Binary(..), encode)
import Data.ByteString.Lazy (toStrict)


data Block = Block { previous     :: Hash Id
                   , timestamp    :: UTCTime
                   , transactions :: [Transaction]
                   , baseTarget   :: Integer
                   , generator    :: PublicKey
                   , genSignature :: Signature
                   } deriving (Show, Generic)

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
                              , "genSignature" .= genSignature b
                              ]
