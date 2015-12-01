module Network.BitFunctor.Crypto.Types where

import qualified Crypto.PubKey.Ed25519 as Ed25519


type SecretKey = Ed25519.SecretKey
type PublicKey = Ed25519.PublicKey
type Signature = Ed25519.Signature
