module Network.BitFunctor.Transaction ( Transaction (..)
                                      , TxInput (..)
                                      , TxInputType (..)
                                      , TxOutput (..)
                                      , TransactionHash
                                      , sign
                                      , verify
                                      , value
                                      , from
                                      , to
                                      , validateHeader
                                      ) where

import qualified Crypto.PubKey.Ed25519 as C (sign, verify)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary (encode)

import Network.BitFunctor.Transaction.Types
import Network.BitFunctor.Account
import Network.BitFunctor.Token (BTF (..), (+.))


sign :: Account -> Transaction -> Maybe Transaction
sign acc tx = case from tx == toAccountId acc of
                True  -> do
                  sk <- secKey acc
                  let pk = pubKey acc
                  return tx { signature = C.sign sk pk (signEncode tx) }
                False -> Nothing

verify :: Transaction -> Bool
verify tx = C.verify (pubKey . fromAccountId $ from tx)
                     (signEncode tx) $ signature tx


value :: Transaction -> BTF
value tx = fee tx +. case inputType $ input tx of
                       Value { amount = amnt } -> amnt
                       _ -> BTF 0

from :: Transaction -> AccountId
from = sender . input

to :: Transaction -> AccountId
to = recipient . output

validateHeader :: Transaction -> Bool
validateHeader tx = worthwhile && signatureValid
                  where worthwhile     = value tx >= BTF 0
                        signatureValid = verify tx

signEncode :: Transaction -> B.ByteString
signEncode = BL.toStrict . encode . TransactionSigning

