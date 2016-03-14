module Network.BitFunctor.Forger where

import Network.BitFunctor.Blockchain as B
import Network.BitFunctor.MemPool as MP
import Network.BitFunctor.Consensus as C

import qualified Data.Map as M


forge :: Account -> [Hash Id]
forge = undefined -- forever $ wait 10sec $ forgeOnce $ for all B.tips

forgeOnce :: Account -> Hash Id -> Timestamp -> (Block -> Block -> ()) -> LocalState -> LocalState -> Maybe Block
forgeOnce acct pb ts pushBlock state = do
  let txs  = MP.transactions memPool -- TODO: ensure valid ordering, MP can contain dependent (tokens and/or theory) txs
  let effb = B.effectiveBalance state pb acct
  let hit  = C.calculateHit pb acct
  let openb = openBlocks state
  if C.verifyHit hit pb ts effb then
      newb <- newBlock pb acct txs (C.nextBaseTarget state ts, ts)
      pushBlock pb newb   -- async(?) call on node/core
      return newb
    else
      -- state {openBlocks = addSortedBlock pb openb}
      return Nothing


-- use these ideas to do filtering, like tips we can forge on, etc.
forgeBlocks ::  Timestamp -> Node -> Node
forgeBlocks ts node = let acc = account node in
                      let view = localView node in
                      let opb = openBlocks node in
                      let (blocks, rb) = splitBlocks (tfdepth acc) opb in
                      let bs = filter (\b -> totalDifficulty b >= diffThreshold view) blocks in
                      let node' = node {openBlocks = []} in
                      foldl (\n pb -> forgeBlock pb n ts) node' blocks
