{-# LANGUAGE TypeSynonymInstances #-}

module Network.BitFunctor.Crypto.Arbitrary where

import Network.BitFunctor.Crypto.Types

import Data.ByteArray (pack, Bytes)
import Data.Word
import Crypto.PubKey.Ed25519 (secretKey)
import Crypto.Error

import Test.QuickCheck
import Test.QuickCheck.Instances


instance Arbitrary SecretKey where
  arbitrary = do
    let secKeyLength = 32 -- cryptonite doesn't export that...
    bytes <- vectorOf secKeyLength (arbitrary :: Gen Word8)
    return . throwCryptoError . secretKey $ (pack bytes :: Bytes)
