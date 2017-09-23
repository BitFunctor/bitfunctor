{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Params

import qualified Network.BitFunctor.Identifiable as Id
import Network.BitFunctor2.Platform.Blockchain as BC
import Network.BitFunctor2.Platform.Blockchain.Types
import Network.BitFunctor2.Platform.Blockchain.Consensus
import Network.BitFunctor2.Platform.Blockchain.Consensus.Types

import Control.Monad.Except


main :: IO ()
main = do
  putStrLn "== BitFunctor Platform Genesis Block Generator =="

  runBlockchainApp

  return ()

runBlockchainApp = runBlockchain b c
  where b :: BlockchainApp ()
        b = genesisBlockchain >> testBlockchain
        c = BlockchainConfig {}


genesisBlockchain :: BlockchainApp ()
genesisBlockchain = forgeOnce

testBlockchain :: BlockchainApp ()
testBlockchain = replicateM_ 5 forgeOnce


forgeOnce :: Blockchain b => b ()
forgeOnce = generateNewBlock >>= newBlock

forge :: Blockchain b => b ()
forge = forever forgeOnce


generateNewBlock :: Blockchain b => b NewBlockFullInfo
generateNewBlock = do
  bs <- tips
  let prevBlocks = case bs of
                     []        -> Nothing
                     otherwise -> prevBlockSelectionPolicy bs
  let blockStub = Block { blockMeta = BlockMeta { blockMetaPrevId = prevBlocks }
                        , blockConsensus = ConsensusData {}
                        , blockTransactions = []
                        }
  let nbfi = NewBlockFullInfo { nbfiId    = Id.id blockStub
                              , nbfiBlock = blockStub
                              , nbfiTxs   = []
                              }

  consensusData <- consensusForBlock nbfi
  let block = blockStub { blockConsensus = consensusData }
  return $ NewBlockFullInfo { nbfiId    = Id.id block
                            , nbfiBlock = block
                            , nbfiTxs   = []
                            }
    where prevBlockSelectionPolicy _ = Nothing

-- | Call <Backend>.generateConsensus, and then does
-- appropriate consensus error type to BlockchainError type conversion
consensusForBlock :: Blockchain b => NewBlockFullInfo -> b ConsensusData
consensusForBlock nbfi = generateConsensus nbfi >>= either consensusErr return
  where consensusErr e = throwError $ ConsensusGenerationError "Can't generate consensus data"
