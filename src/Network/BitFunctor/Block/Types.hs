{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.BitFunctor.Block.Types ( Block (..)
                                      , BlockHash
                                      , BlockSigning (..)
                                      ) where

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
import Network.BitFunctor.Crypto.Signing
import Network.BitFunctor.Common (UTCTimeAsPOSIXSeconds (..))

import Data.Binary as Bin (Binary(..), encode)
import Data.ByteString.Lazy (toStrict)
import Data.Word (Word8)



type BlockHash = Hash Id Block


data Block = Block { previous     :: Hash Id Block
                   , timestamp    :: UTCTime
                   , transactions :: [Hash Id Transaction]
                   , baseTarget   :: Integer
                   , generator    :: AccountId
                   , signature    :: Signature
                   } deriving (Show, Eq, Generic)


instance Binary Block where
  put block = do
    put (0 :: Word8)
    putBlockOptSig True block
  get = do
    tag <- get
    case tag :: Word8 of
      0 -> do
        p <- get
        utcFromPOSIXT <- get
        txs <- get
        tgt <- get
        g <- get
        s <- get
        let (UTCTimeAsPOSIXSeconds t) = utcFromPOSIXT
        return $ Block p t txs tgt g s
      _ -> fail "binary: can't parse block (wrong tag)"


instance Identifiable Block where
  id = hash . toStrict . Bin.encode


newtype BlockSigning = BlockSigning Block

instance Binary BlockSigning where
  put (BlockSigning block) = putBlockOptSig False block
  get = fail "No binary parsing for BlockSigning"


putBlockOptSig withSig block = do
  put $ previous block
  put $ UTCTimeAsPOSIXSeconds $ timestamp block
  put $ transactions block
  put $ baseTarget block
  put $ generator block
  case withSig of
    True  -> put $ signature block
    False -> return ()


-- instance FromJSON Block

instance ToJSON Block where
  toJSON b@(Block{}) = object [ "transactions" .= transactions b
                              , "timestamp"    .= timestamp b
                              , "baseTarget"   .= baseTarget b
                              , "generator"    .= generator b
                              , "signature"    .= signature b
                              ]

