module Network.BitFunctor.Transaction ( Transaction (..)
                                      , sign
                                      , verify
                                      , value
                                      ) where

import qualified Crypto.PubKey.Ed25519 as C (sign, verify)
import qualified Data.ByteString as B

import Network.BitFunctor.Transaction.Types
import Network.BitFunctor.Account
import Network.BitFunctor.Token (BTF, (+.))


sign :: Account -> Transaction -> Maybe Transaction
sign acc tx@(Transaction {sender = s}) | s == toAccountId acc = do
  sk <- secKey acc
  let pk = pubKey acc
  return tx { signature = C.sign sk pk (signEncode tx) }
sign acc tx = Nothing

verify :: Transaction -> Bool
verify tx = C.verify (pubKey . fromAccountId $ sender tx)
                     (signEncode tx) $ signature tx


value :: Transaction -> BTF
value tx = amount tx +. fee tx


signEncode :: Transaction -> B.ByteString
signEncode tx = undefined
