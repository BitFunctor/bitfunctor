module Network.BitFunctor.MemoryPool ( 
                                     -- PendingTxStorage (..)
                                     -- , 
                                     MemoryPool
                                     ) where

import Data.Time.Clock (UTCTime)
import Network.BitFunctor.Transaction
import Network.BitFunctor.Identifiable as I
import Network.BitFunctor.State.Ledger

import qualified Data.Map as M


-- data TxValidationResult = Ok

-- data TxApplicationFailure = InsufficientBalance
--                           | Other

-- data TxApplicationResult = Applied -- add ledger id field?
--                          | NotApplied { reason :: TxApplicationFailure }

-- data TxVerificationContext = TxVerificationContext {
--                               struct :: Maybe TxStructuralValidityContext,
--                               ledgerConsistency :: [(Hash Id, TxApplicationResult)]
-- }

-- TxCheckResult
-- TxCheckProof

-- data BlockApplicationFailure = TxApplicationFailure
--                              | Other

-- data BlockApplicationResult = Applied
--                             | NotApplied { reason :: BlockApplicationFailure }



data TransactionMeta = TransactionMeta {
  transaction   :: Transaction,
  transactionId :: TransactionHash
}

data MemoryPool = MemoryPool {
  transactions :: M.Map TransactionHash TransactionMeta
}


-- instance PendingTxs MemoryPool where
--   addTx mp le tx = mp { transactions = M.insert txId txMeta
--                                                 (transactions mp)
--                       }
--                  where
--                    txId   = I.id tx
--                    txMeta = TransactionMeta { transaction   = tx
--                                             , transactionId = txId
--                                             }
--   takeTx = undefined
--   getTx  = undefined
--   getTxs = undefined

