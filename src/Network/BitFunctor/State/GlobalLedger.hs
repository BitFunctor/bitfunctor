module Network.BitFunctor.State.GlobalLedger ( GlobalLedger
                                             , accountBalance
                                             , ledger
                                             ) where

import qualified Network.BitFunctor.State.Ledger as L
import Network.BitFunctor.Account
import Network.BitFunctor.Asset
import Network.BitFunctor.Block

import qualified Data.Map as M


type GlobalLedger = M.Map BlockHash L.Ledger


accountBalance :: GlobalLedger -> BlockHash -> AccountId -> Maybe BTF
accountBalance gl h acc = do
  le <- ledger gl h
  return $ L.accountBalance le acc

ledger :: GlobalLedger -> BlockHash -> Maybe L.Ledger
ledger = flip M.lookup
