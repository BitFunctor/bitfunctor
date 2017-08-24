{-# LANGUAGE TypeSynonymInstances #-}

module Network.BitFunctor.Crypto.Signing.Types (Signature) where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import Crypto.Error

import Data.Aeson
import Data.Binary

import Data.ByteArray (convert, pack, Bytes)
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B



type Signature = Ed25519.Signature


instance ToJSON Signature where
  toJSON = String . TE.decodeUtf8 . B16.encode . convert

instance Binary Signature where
  put sig = put (convert sig :: B.ByteString)
  get     = do
    bytes <- get
    return . throwCryptoError $ Ed25519.signature (pack bytes :: Bytes)
