module Network.BitFunctor.State.Pending where

import Network.BitFunctor.Transaction


class PendingTxs mp where
  addTx  :: mp -> Transaction -> mp
  takeTx :: mp -> TransactionHash -> (Maybe Transaction, mp)
  getTx  :: mp -> TransactionHash -> Maybe Transaction
  getTxs :: Foldable t => mp -> t Transaction
