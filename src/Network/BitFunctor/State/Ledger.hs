module Network.BitFunctor.State.Ledger ( Ledger
                                       , accountBalance
                                       , accountOptions
                                       , applyTxs
                                       , validateTx
                                       ) where

import qualified Data.Map as M
import Control.Monad (foldM)

import Network.BitFunctor.Account
import Network.BitFunctor.Transaction as Tx
import Network.BitFunctor.Asset
import Network.BitFunctor.Identifiable as I


data Ledger = Ledger {
  tokens  :: M.Map AccountId BTF,
  cTokens :: M.Map AccountId [CBTF]
  -- theory
} deriving Show


accountBalance :: Ledger -> AccountId -> BTF
accountBalance le acc = M.findWithDefault 0 acc (tokens le)

accountOptions :: Ledger -> AccountId -> [CBTF]
accountOptions le acc = M.findWithDefault [] acc (cTokens le)

applyTxs :: Ledger -> AccountId -> [Transaction] -> Maybe Ledger
applyTxs le feeRcv = foldM (\le tx -> case validateTx le tx of
                                        False -> Nothing
                                        True  -> applyTx le feeRcv tx) le


applyTx :: Ledger -> AccountId -> Transaction -> Maybe Ledger
applyTx le feeRcv tx = do
  let from = Tx.from tx
  let to   = Tx.to   tx
  return $ payFee le from feeRcv (fee tx)
  return $ case inputType $ input tx of
    (Value        { amount = amnt }) -> moveTokens le from to amnt
    (Option       { option = opt })  -> moveOptions le from to opt
    (OptionCreate { payload = pld }) -> createOption le to (CBTF $ I.id tx)
    (OptionBurn   { claimOption = opt, claimAmount = amnt }) -> burnOption le opt amnt to


validateTx :: Ledger -> Transaction -> Bool
validateTx le tx = Tx.validateHeader tx && tokenTransferValid le tx -- && validatePayload


payFee :: Ledger -> AccountId -> AccountId -> BTF -> Ledger
payFee = moveTokens


tokenTransferValid :: Ledger -> Transaction -> Bool
tokenTransferValid le tx = accountBalance le (Tx.from tx) >= Tx.value tx




moveTokens :: Ledger -> AccountId -> AccountId -> BTF -> Ledger
moveTokens le from to value = le { tokens =   M.insert to   (balanceTo   + value)
                                            . M.insert from (balanceFrom - value)
                                            $ tokens le
                                 } -- TODO: purge accounts with 0 balance
                              where balanceFrom = accountBalance le from
                                    balanceTo   = accountBalance le to

moveOptions :: Ledger -> AccountId -> AccountId -> CBTF -> Ledger
moveOptions le from to option = le { cTokens =   M.insert to   toTokens
                                               . M.insert from fromTokens
                                               $ cTokens le }
                              where fromTokens = accountOptions le from -- \\ option
                                    toTokens   = accountOptions le to   -- ++ option

createOption :: Ledger -> AccountId -> CBTF -> Ledger
createOption le acc option = le { cTokens = M.insert acc options (cTokens le) }
                           where options = option : accountOptions le acc

burnOption :: Ledger -> CBTF -> BTF -> AccountId -> Ledger
burnOption le token recipient = undefined




tokenValue :: Ledger -> CBTF -> BTF
tokenValue le token = undefined
