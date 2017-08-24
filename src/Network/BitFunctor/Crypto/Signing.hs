module Network.BitFunctor.Crypto.Signing ( Signature
                                         , sign
                                         , verify
                                         ) where


import Network.BitFunctor.Crypto.Signing.Types
import Network.BitFunctor.Crypto.Key.Types

import qualified Crypto.PubKey.Ed25519 as C (sign, verify)
import Data.ByteArray


sign :: ByteArrayAccess ba => SecretKey -> PublicKey -> ba -> Signature
sign = C.sign

verify :: ByteArrayAccess ba => PublicKey -> ba -> Signature -> Bool
verify = C.verify
