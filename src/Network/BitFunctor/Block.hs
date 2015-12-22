module Network.BitFunctor.Block (newBlock, signBlock) where

import Network.BitFunctor.Block.Types
import Network.BitFunctor.Crypto.Hash

import Data.Time.Clock
import Crypto.PubKey.Ed25519 (sign)
import qualified Data.ByteString as B
import Network.BitFunctor.Account


newBlock :: Hash Id -> Account -> [Hash Id] -> (Integer, UTCTime) -> Maybe Block
newBlock prevBlockId gen txsIds (bt, time) = do
  signBlock gen $ empty { previous     = prevBlockId
                        , timestamp    = time
                        , transactions = txsIds
                        , baseTarget   = bt
                        }


signBlock :: Account -> Block -> Maybe Block
signBlock acc block = do
  sk <- secKey acc
  let pk = pubKey acc
  let sigBlockBin = signEncode block
  return block { generator    = pk
               , genSignature = sign sk pk sigBlockBin
               }


signEncode :: Block -> B.ByteString
signEncode = undefined

empty :: Block
empty = Block { }
