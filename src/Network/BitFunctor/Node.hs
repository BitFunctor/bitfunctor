module Network.BitFunctor.Node ( processBlock
                               , processTx
                               , getBlocks
                               , getTransactions
                               ) where

import Network.BitFunctor.Blockchain
import Network.BitFunctor.MemoryPool


data NodeState = NodeState { ledger     :: Blockchain
                             pendingTxs :: MemoryPool
                           } deriving (Show)



processBlock :: Block -> Currency
--processBlock :: Block -> LocalState -> LocalState
processBlock b = do

processIncomingBlock :: Node -> Block -> Block -> Node
processIncomingBlock node pb block = updNode
    where
        sigHash = first8bytesAsNumber $ calcGenerationSignature pb (generator block)
        -- todo: many other checks!!
        updNode = case first8bytesAsNumber (generationSignature block) == sigHash of
            True -> pushBlock node pb block
            False -> node

processTx :: Transaction -> Currency
processTx tx = do

getBlocks :: Currency -> [Hash Id] -> [Maybe Block]
getBlocks = undefined

getTransactions :: Currency -> [Hash Id] -> [Maybe Transaction]
getTransactions = undefined

getTips :: Currency -> [Hash Id]
-- or getTips :: Currency -> [(Hash Id, CumDiff)] ?
getTips = undefined
