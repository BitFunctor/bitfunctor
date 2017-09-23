module Network.BitFunctor.State where

import Network.BitFunctor.Block
import Network.BitFunctor.Transaction
import Network.BitFunctor.Asset


type Assets = (BTF, CBTF)

class Ledger le a where
  assets     :: Asset a => le -> AccountId -> Maybe a
  applyTx    :: le -> Transaction -> (TxApplicationResult, le)
  applyTxs :: Foldable t => le -> t Transaction -> (TxApplicationResult, le)
  applyTxs le = foldl (\(r,le) tx -> case r of
                        Applied    -> applyTx le tx
                        notApplied -> (notApplied, le)) (Applied, le)
  -- TODO: where does actually this belong?
  -- valuates asset in some underlying assets
  -- id for BTF asset, obviously
  valuation :: Asset a => le -> a -> Integer


class GlobalLedger gl le where
  assets :: gl -> BlockHash -> AccountId -> Maybe Assets
  ledger :: gl -> BlockHash -> Maybe le





class LocalState ls gl mp where
  apply   :: Block -> (BlockApplicationResult, ls)

  ledger  :: ls -> gl
  pending :: ls -> mp
