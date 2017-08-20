{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.BitFunctor.Transaction.Types ( Transaction (..)
                                            , TxInput (..)
                                            , TxInputType (..)
                                            , TxOutput (..)
                                            , TransactionHash
                                            , TransactionSigning (..)
                                            , TheoryPayload (..)
                                            ) where

import Data.Time.Clock (UTCTime)
import GHC.Generics

import Network.BitFunctor.Account
import Network.BitFunctor.Asset
import Network.BitFunctor.Crypto.Types
import Network.BitFunctor.Crypto.Hash (hash, Hash, Id)
import Network.BitFunctor.Identifiable
-- import qualified Network.BitFunctor.Theory.Types as Theory
import Network.BitFunctor.Common (UTCTimeAsPOSIXSeconds (..))

import Data.Binary as Binary (Binary(..), encode)

import Data.ByteString.Lazy (toStrict)
import Data.Word (Word8)



type TransactionHash = Hash Id Transaction


data TxInput = TxInput { sender    :: AccountId
                       , inputType :: TxInputType
                       } deriving (Show, Eq, Generic)

-- TODO : add announcement of code and services  
data TxInputType = Value        { amount      :: BTF } -- transfer BTF
                 | Option       { option      :: CBTF (Hash Id Transaction) } -- transfer CBTF
                 | OptionCreate { payload     :: TheoryPayload } -- send a code  -> create option
                 | OptionBurn   { claimOption :: CBTF (Hash Id Transaction) -- exchange option for BTF
                                , claimAmount :: BTF -- the exact value (this or nothing) sender wants back
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
  uses :: [Hash Id Transaction]
 -- , code :: Theory.Code
} deriving (Eq, Show, Generic)


instance Identifiable Transaction where
  id = hash . toStrict . Binary.encode

instance Binary Transaction where
  put tx = do
    put (0 :: Word8)
    putTxOptSig True tx
  get = do
    tag <- get
    case tag :: Word8 of
      0 -> do
        i <- get
        o <- get
        f <- get
        utcFromPOSIXT <- get
        s <- get
        let (UTCTimeAsPOSIXSeconds t) = utcFromPOSIXT
        return $ Transaction i o f t s
      _ -> fail "binary: can't parse tx (wrong tag)"


newtype TransactionSigning = TransactionSigning Transaction

instance Binary TransactionSigning where
  put (TransactionSigning tx) = putTxOptSig False tx
  get = fail "No binary parsing for TransactionSigning"


putTxOptSig withSig tx = do
    put $ input tx
    put $ output tx
    put $ fee tx
    put $ UTCTimeAsPOSIXSeconds $ timestamp tx
    case withSig of
      False -> return ()
      True  -> put $ signature tx


instance Binary TheoryPayload where
  put tp = put (0 :: Word8)           -- STUB/TODO:
  get    = return $ TheoryPayload []  -- STUB/TODO:

instance Binary TxInputType where
  put (Value  amt) = put (0 :: Word8) >>= \_ -> put amt
  put (Option opt) = put (1 :: Word8) >>= \_ -> put opt
  put (OptionCreate pld)       = put (2 :: Word8) >>= \_ -> put pld
  put (OptionBurn   copt camt) = put (3 :: Word8) >>= \_ -> put copt >>= \_ -> put camt
  get = do
    tag <- get
    case tag :: Word8 of
      0 -> do
        amt <- get
        return $ Value amt
      1 -> do
        opt <- get
        return $ Option opt
      2 -> do
        pld <- get
        return $ OptionCreate pld
      3 -> do
        copt <- get
        camt <- get
        return $ OptionBurn copt camt
      _ -> fail "binary: can't parse txinputtype (wrong tag)"

instance Binary TxInput where
  put (TxInput s t) = do
    put (0 :: Word8)
    put s
    put t
  get = do
    tag <- get
    case tag :: Word8 of
      0 -> do
        s <- get
        t <- get
        return $ TxInput s t
      _ -> fail "binary: can't parse txinput (wrong tag)"

instance Binary TxOutput where
  put (TxOutput r) = do
    put (0 :: Word8)
    put r
  get = do
    tag <- get
    case tag :: Word8 of
      0 -> do
        r <- get
        return $ TxOutput r
      _ -> fail "binary: can't parse txoutput (wrong tag)"

