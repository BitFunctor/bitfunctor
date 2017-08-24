{-# LANGUAGE TypeSynonymInstances #-}

module Network.BitFunctor.Crypto.Key.Types ( SecretKey
                                           , PublicKey
                                           ) where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import Crypto.Error

import Data.Aeson
import Data.Binary

import Data.ByteArray (convert, pack, Bytes)
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B



type SecretKey = Ed25519.SecretKey
type PublicKey = Ed25519.PublicKey


instance ToJSON PublicKey where
  toJSON = String . TE.decodeUtf8 . B16.encode . convert

instance Binary PublicKey where
  put pk = put (convert pk :: B.ByteString)
  get    = do
    bytes <- get
    return . throwCryptoError $ Ed25519.publicKey (pack bytes :: Bytes)
