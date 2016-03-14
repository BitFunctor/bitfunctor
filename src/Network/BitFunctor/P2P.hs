module Network.BitFunctor.P2P where

import Network.BitFunctor.Block
import Network.BitFunctor.Transaction


relayBlock :: Block -> CurrencyP2P
relayBlock = undefined

relayTx :: Transaction -> CurrencyP2P
relayTx = undefined

requestBlocks :: [Hash Id] -> ([Hash Id] -> ()) -> CurrencyP2P
requestBlocks = undefined

requestTransactions :: [Hash Id] -> ([Hash Id] -> ()) -> CurrencyP2P
requestTransactions = undefined
