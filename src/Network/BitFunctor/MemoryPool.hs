module Network.BitFunctor.MemoryPool ( MemoryPool
                                     ) where

import Data.Time.Clock (UTCTime)
import Network.BitFunctor.Transaction
import Network.BitFunctor.Identifiable as I

import qualified Data.Map as M


data TransactionMeta = TransactionMeta {
  transaction   :: Transaction,
  transactionId :: TransactionHash
  firstSeen     :: UTCTime,
  lastSeen      :: UTCTime
}

data MemoryPool = MemoryPool {
  transactions :: M.Map TransactionHash TransactionMeta
}


takeTx :: MemoryPool -> TransactionHash -> (Maybe Transaction, MemoryPool)
takeTx = undefined

addTx :: MemoryPool -> Ledger -> Transaction -> (Bool, MemoryPool)
addTx mp le tx | validateTx le tx = (True,  mp')
               | otherwise        = (False, mp)
               where
                 mp' = mp { transactions = M.insert txId txMeta
                                                    (transactions mp)
                          }
                 txId   = (I.id tx)
                 txMeta = TransactionMeta { transaction   = tx
                                          , transactionId = txId
                                          , firstSeen     = 0
                                          , lastSeen      = 0
                                          }


getTxs :: MemoryPool -> [Transaction]
getTxs = undefined

getTx :: MemoryPool -> TransactionHash -> Maybe Transaction
getTx = undefined
