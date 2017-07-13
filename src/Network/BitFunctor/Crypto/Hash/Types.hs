{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, StandaloneDeriving #-}

module Network.BitFunctor.Crypto.Hash.Types ( HashAlgorithm (..)
                                            , Id
                                            , Keccak_256
                                            , Hash (..)
                                            , toString
                                            ) where

import Crypto.Hash.Algorithms (HashAlgorithm, Keccak_256)
import Crypto.Hash (hash, Digest (..), digestFromByteString)

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (typeMismatch, defaultOptions)
import Data.Binary (Binary(..))

import Data.ByteArray (convert)
import Data.ByteString as B (ByteString, null)
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.Text as DT (unpack, Text (..))
import qualified Data.Text.Encoding as TE
import qualified Data.Serialize as DS

type Id = Keccak_256

data Hash a = Hash (Digest a)
              deriving (Eq, Ord, Show, Generic)

instance HashAlgorithm a => ToJSON (Digest a) where
  toJSON = toJSON . TE.decodeUtf8 . B16.encode . convert

instance HashAlgorithm a => FromJSON (Digest a) where
  parseJSON v@(String t) = do
                             (tt :: DT.Text) <- parseJSON v
                             let (bytes, failbytes) = B16.decode $ TE.encodeUtf8 tt
                             if not . B.null $ failbytes then
                               typeMismatch "fromjson: can't parse digest (unparsable substring)" v
                             else case digestFromByteString (bytes :: ByteString) of
                               Just d  -> return d
                               Nothing -> typeMismatch "fromjson: can't parse digest" v
  parseJSON q = typeMismatch "fromjson: digest type mismatch" q


instance (HashAlgorithm a) => FromJSON (Hash a)
instance (HashAlgorithm a) => ToJSON (Hash a) where
  toJSON = genericToJSON defaultOptions

instance (HashAlgorithm a) => Binary (Digest a) where
  put d = put (convert d :: ByteString)
  get = do
    bytes <- get
    case digestFromByteString (bytes :: ByteString) of
      Just d  -> return d
      Nothing -> fail "binary: can't parse digest"

instance (HashAlgorithm a) => Binary (Hash a) where
  put (Hash digest) = put digest
  get = get >>= \algo -> return $ Hash algo


instance DS.Serialize (Digest Id) where
  put d = DS.put (convert d :: ByteString)
  get   = do
    bytes <- DS.get
    case digestFromByteString (bytes :: ByteString) of
      Just d  -> return d
      Nothing -> fail "binary: can't parse digest"

instance DS.Serialize (Hash Id) where
  put (Hash digest) = DS.put digest
  get = DS.get >>= \algo -> return (Hash algo)

toString (Hash d) = DT.unpack . TE.decodeUtf8 . B16.encode $ convert d
