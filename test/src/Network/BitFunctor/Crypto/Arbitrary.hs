{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.BitFunctor.Crypto.Arbitrary where

import Network.BitFunctor.Crypto.Hash
import Network.BitFunctor.Crypto.Types

import Data.ByteArray (pack, Bytes)
import Data.Word
import Crypto.PubKey.Ed25519 (secretKey, toPublic, sign)
import Crypto.Error

import Test.QuickCheck
import Test.QuickCheck.Instances


instance Arbitrary SecretKey where
  arbitrary = do
    let secKeyLength = 32 -- cryptonite doesn't export that...
    bytes <- vectorOf secKeyLength (arbitrary :: Gen Word8)
    return . throwCryptoError $ secretKey (pack bytes :: Bytes)

instance Arbitrary PublicKey where
  arbitrary = do
    sk <- arbitrary
    return $ toPublic sk

instance Arbitrary Signature where
  arbitrary = do
    sk <- arbitrary
    let pk = toPublic sk
    bytes <- vectorOf 1 (arbitrary :: Gen Word8)
    return $ sign sk pk (pack bytes :: Bytes)

instance (HashAlgorithm a) => Arbitrary (Hash a) where
  arbitrary = do
    bytes <- vectorOf 1 (arbitrary :: Gen Word8)
    return $ hash (pack bytes :: Bytes)
