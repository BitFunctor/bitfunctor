module Network.BitFunctor.Transaction ( Transaction (..)
                                      , sign
                                      , verify
                                      ) where

import qualified Crypto.PubKey.Ed25519 as C (sign, verify)
import qualified Data.ByteString as B

import Network.BitFunctor.Transaction.Types
import Network.BitFunctor.Account


sign :: Account -> Transaction -> Maybe Transaction
sign acc tx@(Transaction {sender = s}) | s == toAccountId acc = do
  sk <- secKey acc
  let pk = pubKey acc
  return tx { signature = C.sign sk pk (signEncode tx) }
sign acc tx = Nothing

verify :: Transaction -> Bool
verify tx = C.verify (pubKey . fromAccountId $ sender tx)
                     (signEncode tx) $ signature tx


signEncode :: Transaction -> B.ByteString
signEncode tx = undefined
