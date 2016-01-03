module Network.BitFunctor.State.Ledger ( Ledger
                                       , accountBalance
                                       , applyTxs
                                       , validateTx
                                       ) where

import qualified Data.Map as M
import Control.Monad (foldM)

import Network.BitFunctor.Account
import Network.BitFunctor.Transaction as Tx
import Network.BitFunctor.Token
import Network.BitFunctor.Identifiable as I


data Ledger = Ledger {
  tokens  :: M.Map AccountId BTF,
  cTokens :: M.Map AccountId [CBTF]
} deriving Show


accountBalance :: Ledger -> AccountId -> BTF
accountBalance le acc = M.findWithDefault (BTF 0) acc (tokens le)

applyTxs :: Ledger -> AccountId -> [Transaction] -> Maybe Ledger
applyTxs le feeRcv = foldM (flip applyTx feeRcv) le


applyTx :: Ledger -> AccountId -> Transaction -> Maybe Ledger
applyTx le feeRcv tx | validateTx le tx = return le'''
                     -- charge (move) fee   from the sender (to the block signer)
                     -- charge (move) value from the sender (to the recipient)
                     -- assign cBTF tokens if contribution tx
                     | otherwise        = Nothing
                     where s     = sender tx
                           le'   = moveTokens le   s feeRcv         (fee tx)
                           le''  = moveTokens le'  s (recipient tx) (amount tx)
                           le''' = emitToken  le'' s (CBTF $ I.id tx) -- TODO: only if contribution tx

validateTx :: Ledger -> Transaction -> Bool
validateTx le tx = Tx.validateHeader tx && tokenTransferValid le tx


tokenTransferValid :: Ledger -> Transaction -> Bool
tokenTransferValid le tx = accountBalance le (sender tx) >= Tx.value tx


moveTokens :: Ledger -> AccountId -> AccountId -> BTF -> Ledger
moveTokens le from to value = le { tokens =   M.insert to   (balanceTo   +. value)
                                            . M.insert from (balanceFrom -. value)
                                            $ tokens le
                                 } -- TODO: purge accounts with 0 balance
                              where balanceFrom = accountBalance le from
                                    balanceTo   = accountBalance le to


emitTokens :: Ledger -> AccountId -> BTF -> Ledger
emitTokens le acc value = le { tokens = M.insert acc (accountBalance le acc +. value)
                                                     (tokens le)
                             }

emitToken :: Ledger -> AccountId -> CBTF -> Ledger
emitToken le acc token = undefined

transformToken :: Ledger -> CBTF -> AccountId -> Ledger
transformToken le token recipient = undefined

tokenValue :: Ledger -> CBTF -> BTF
tokenValue le token = undefined
