module Network.BitFunctor.Block ( Block (..)
                                , BlockHash
                                , new
                                , sign
                                , verify
                                ) where

import qualified Crypto.PubKey.Ed25519 as C (sign, verify)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary (encode)
import Data.Time.Clock

import Network.BitFunctor.Block.Types
import Network.BitFunctor.Crypto.Hash
import Network.BitFunctor.Account



new :: Hash Id -> Account -> [Hash Id] -> (Integer, UTCTime) -> Maybe Block
new prevBlockId gen txsIds (bt, time) = do
  sign gen $ empty { previous     = prevBlockId
                   , timestamp    = time
                   , transactions = txsIds
                   , baseTarget   = bt
                   }

sign :: Account -> Block -> Maybe Block
sign acc block = do
  sk <- secKey acc
  let sigBlockBin = signEncode block
  return block { generator = toAccountId acc
               , signature = C.sign sk (pubKey acc) sigBlockBin
               }

verify :: Block -> Bool
verify b = C.verify (pubKey . fromAccountId $ generator b)
                    (signEncode b) $ signature b


signEncode :: Block -> B.ByteString
signEncode = BL.toStrict . encode . BlockSigning


empty :: Block
empty = Block { }
