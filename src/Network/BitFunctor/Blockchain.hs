module Network.BitFunctor.Blockchain ( block
                                     , bestChain
                                     , processBlock
                                     ) where

import Network.BitFunctor.Block       as Block
import Network.BitFunctor.Transaction as Tx
import Network.BitFunctor.Consensus   as Consensus
import Network.BitFunctor.MemoryPool  as MemPool

import Data.Binary (encode)
import Network.BitFunctor.Theory.Types

import Control.Monad (foldM, mapM)
import Data.Maybe (isJust)

import Network.BitFunctor.State.Ledger as L
import Network.BitFunctor.State.GlobalLedger as G

import qualified Data.Map as M


newtype BlockHeight = Integer
newtype TransactionId = Hash (Hash Id)


data BlockId = Hash    BlockHash
             | Height  BlockHeight
             | Genesis
             | Tip
             | HashOffset BlockHash Integer
             deriving Show

-- from the next to parent
--type BlockTree = Map.Map BlockId BlockId

type Blockchain = [BlockMeta]
-- all possible blockchains, i.e. all paths from the root to every leaf of the tree
type Blocktree  = [Blockchain]

data Blocktree' = Chain BlockMeta Blocktree'
                | Fork Blocktree' Blocktree'


data BlockMeta = BlockMeta {
  height   :: BlockHeight,
  cumScore :: Integer,
  block    :: Block
}

data LocalState = LocalState {
  --blockTree :: BlockTree,
  --diffThreshold :: Double,
  --blockBalances :: Map.Map Block Ledger,
  --blockTheory :: Map.Map Block Theory,
  --bestBlock :: Block,
  --blockTransactions :: Map.Map Block [Transaction]

  blockchain   :: Blockchain,
  blocks       :: Map.Map BlockHash BlockMeta,
  transactions :: Map.Map TransactionHash Transaction,
  -- blockIds     :: Map.Map BlockHeight [Hash Id]
  ledger       :: GlobalLedger,
  pending      :: MemoryPool
} deriving (Show)

-------------

block :: LocalState -> BlockId -> Maybe Block
block s (Hash h)   = M.lookup h (blocks s)
block s (Height h) = undefined -- Height to Hash for the best blockchain and then lookup with block id

transaction :: LocalState -> TransactionId -> Maybe Transaction
transaction s (Hash h) = M.lookup h (transactions s)

bestChain :: LocalState -> Blockchain
bestChain s = blockchain s
--bestChain s = treeChain (bestBlock s) (blockTree s)


tips :: LocalState -> [BlockHash]
tips = undefined

blockTxs :: LocalState -> BlockId -> Maybe [Transaction]
blockTxs = undefined

blockTheory :: LocalState -> BlockId -> Maybe [Theory]
blockTheory = undefined


-------------

accountBalance :: LocalState -> AccountId -> BTF
accountBalance = flip accountBalanceAt Tip

accountBalanceAt :: LocalState -> BlockId -> AccountId -> BTF
accountBalanceAt s (Hash h) acc = G.accountBalance h acc

-------------

applyBlock :: LocalState -> Block -> Maybe LocalState
applyBlock s b | validateBlockHeader s b = do
  s' <- applyBlockTxs s b
  -- TODO: add block to the blockchain
  -- TODO: make block the new tip
               | otherwise               = Nothing

-------------

applyBlockTxs :: LocalState -> Block -> Maybe LocalState
applyBlockTxs s b = do
 currentLedger <- G.ledger (ledger s) (previous b)
 txs <- mapM (\txId -> transaction s txId) (transactions b)
 L.applyTxs currentLedger (generator b) txs

validateBlockHeader :: LocalState -> Block -> Bool
validateBlockHeader s b =    prevBlockValid && isTip
                          && signatureValid && consensusValid
                        where prevBlock      = previous b
                              prevBlockValid = isJust $ block s prevBlock
                              isTip          = elem prevBlock $ tips s
                              signatureValid = Block.verify b
                              consensusValid = Consensus.verify b

validateBlockTxs :: LocalState -> Block -> Bool
validateBlockTxs = isJust . applyBlockTxs

validateBlock :: LocalState -> Block -> Bool
validateBlock = isJust . applyBlock

-------------

--processTheory :: Block -> Theory -> Theory
--processTheory b t = foldl (\t tx -> addAtom (toAtom (payload tx) t) t) t $ transactions b


deltaThreshold = 7

pushBlock :: Node -> Block -> Block -> Node
pushBlock node pb b =  let view = localView node in
                       if (Map.notMember b $ blockTree view) then
                        if (Map.member pb $ blockTree view) || (isGenesis pb) then
                           let prBal = Map.findWithDefault Map.empty pb $ blockBalances view in
                           let prTh = Map.findWithDefault Map.empty pb $ blockTheory view in
                           let prTxs = Map.findWithDefault []       pb $ blockTransactions view in
                           let opb = addSortedBlock b (openBlocks node) in
                           let bb' = head opb in
                           let (newBal, newTh) = processBlock b (prBal, prTh) in
                           let pTxs = pendingTxs node in
                           let updView = view {
                              blockTree         = Map.insert b pb $ blockTree view,
                              blockBalances     = Map.insert b newBal $ blockBalances view,
                              blockTransactions = Map.insert b (prTxs ++ (transactions b)) $ blockTransactions view,
                              blockTheory       = Map.insert b newTh $ blockTheory view,
                              bestBlock =  let oldbb = bestBlock view in
                                            if (totalDifficulty bb' >= totalDifficulty oldbb) then bb' else oldbb,
                              diffThreshold = let olddt = diffThreshold view in
                                              if (totalDifficulty bb' - olddt >= deltaThreshold) then olddt + deltaThreshold
                                                                                                 else olddt} in
                           node {localView = updView, pendingBlocks = (pb,b):(pendingBlocks node),
                                 openBlocks = opb, pendingTxs = filter (\tx -> notElem tx (transactions b)) pTxs }
                        -- filter (\tx -> notElem tx (transactions b)) pTxs
                        -- need to add more logic when prevBlock not found - try to download it or whatever
                        else node
                       else node


nodeChain :: Block -> Node -> BlockChain
nodeChain b node = let view = localView node in
                   let tree = blockTree view in
                   treeChain b tree

treeChain :: Block -> BlockTree -> BlockChain
treeChain b t = if Map.member b t then
                  let pb = Map.findWithDefault b b t in
                           (treeChain pb t) ++ [b]
                else [b]


commonChain :: BlockChain -> BlockChain -> BlockChain
commonChain [] [] = []
commonChain (bl1:ct1) (bl2:ct2) | bl1 == bl2 = bl1 : commonChain ct1 ct2
                                | otherwise  = []


-- what is better (>) or (>=)?
addSortedBlock ::  Block -> [Block] -> [Block]
addSortedBlock b [] = [b]
addSortedBlock b lb@(b':bs) =  if ((totalDifficulty b) >= (totalDifficulty b')) then
                                 b:lb
                               else
                                 b':(addSortedBlock b bs)


-- todo: define canonical blockchain and implement its extraction from blocktree of a system
canonicalBlockchain :: Network -> Maybe BlockChain
canonicalBlockchain sys = Nothing
