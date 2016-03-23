{-# LANGUAGE FlexibleInstances #-}

module Network.BitFunctor.Crypto.Hash ( HashAlgorithm (..)
                                      , Hash
                                      , Id
                                      , hash
                                      ) where

import Network.BitFunctor.Crypto.Hash.Types
-- import Data.ByteArray
import qualified Crypto.Hash as H (hash, Digest)
import Data.Binary (Binary(..)
                    , putWord8
                    , getWord8)
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import Data.ByteArray


type Id = Keccak_256

-- instance Binary Id

instance Binary (H.Digest Id) where
 put _ = putWord8 0
 get  = do
    tag_ <- getWord8
    case tag_ of
      0 -> return undefined
      _ -> fail "no parse"
 
instance Binary (Hash Id) where
   put (Hash d) = put . B16.encode $ convert d
   get = get >>= \a -> return (Hash a)


hash :: (ByteArrayAccess ba, HashAlgorithm a) => ba -> Hash a
hash = Hash . H.hash
