{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.BitFunctor2.Platform.Blockchain.Consensus ( checkConsensus
                                                         , generateConsensus
                                                         , ConsensusError
                                                         ) where

import Network.BitFunctor2.Platform.Blockchain.Types
import qualified Network.BitFunctor2.Platform.Blockchain.Types as BC
import Network.BitFunctor2.Platform.Blockchain.Consensus.Types

import qualified Network.BitFunctor.Identifiable as Id
import Network.BitFunctor.Crypto.Hash

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as BS64
import Data.ByteArray (convert)

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Maybe

import qualified Data.Bool as B
import Data.Foldable (maximumBy)
import Data.Ord (Ordering (..))


data ConsensusError = ConsensusError T.Text
  deriving (Eq, Show)

data ConsensusContext = ConsensusContext {
  cctxNewBlockInfo :: NewBlockFullInfo,
  cctxBlockHeight  :: Int,
  cctxTips         :: [BlockchainBlockInfo]
} deriving (Eq, Show)



newtype ConsensusComputation a = ConsensusComputation {
  unConsensusComputation :: ReaderT ConsensusContext (Except ConsensusError) a
} deriving ( Monad
           , Functor
           , Applicative
           , MonadReader ConsensusContext
           , MonadError ConsensusError
           )


-- looks similar to maybeToExceptT, but it is not
maybeToEffect :: Monad m => m (Maybe a) -> m a -> m a
maybeToEffect x eff = x >>= maybe eff return

-- | The 'extractConsensusContext' function extracts the data required for
-- consensus algorithm operation.
-- This is, basically, the information about the block to achieve consensus on, 
-- plus some algorithm-specific information from the Blockchain
extractConsensusContext :: Blockchain b => NewBlockFullInfo -> b ConsensusContext
extractConsensusContext nbfi = do
  let prevId = blockMetaPrevId . blockMeta $ nbfiBlock nbfi
  h <- maybe (return 0) -- default case – nbfi contains genesis block
             (\x -> maybeToEffect (liftM (fmap (+1)) (BC.height x))
                                  (throwError $ BC.Other "No height for prev block with ID"))
             prevId

  -- https://stackoverflow.com/questions/20293006/how-to-use-maybe-monad-inside-another-monad
  -- https://stackoverflow.com/questions/16064143/maybe-monad-inside-stack-of-transformers
  -- https://en.wikibooks.org/wiki/Haskell/Monad_transformers#A_simple_monad_transformer:_MaybeT
  -- http://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Maybe.html
  -- !! https://stackoverflow.com/questions/32579133/simplest-non-trivial-monad-transformer-example-for-dummies-iomaybe
  -- http://hackage.haskell.org/package/transformers-0.5.4.0/docs/Control-Monad-Trans-Class.html#v:lift

  tHs <- BC.tips
  tBi <- mapM (\bid -> do
                height <- maybeToEffect (BC.height bid)
                                        (throwError $ BC.Other "No height for block with ID")
                block  <- maybeToEffect (BC.block bid)
                                        (throwError $ BC.Other "No block for ID")
                return $ BlockchainBlockInfo bid height block) tHs

  return $ ConsensusContext { cctxNewBlockInfo = nbfi
                            , cctxBlockHeight  = h
                            , cctxTips         = tBi
                            }


runConsensus :: ConsensusComputation a -> ConsensusContext -> Either ConsensusError a
runConsensus c ctx = runExcept $ runReaderT (unConsensusComputation c) ctx


checkConsensus :: Blockchain b => NewBlockFullInfo -> b (Either ConsensusError BlockchainBlockInfo)
checkConsensus nbfi = extractConsensusContext nbfi >>= \ctx -> return $ runConsensus c ctx
  where c :: ConsensusComputation BlockchainBlockInfo
        c = validatePoW >>= selectTip

        validatePoW = do
          h   <- asks cctxBlockHeight
          b   <- asks $ nbfiBlock . cctxNewBlockInfo
          bId <- asks $ nbfiId . cctxNewBlockInfo
          declaredNonce <- asks $ powNonce . blockConsensus . nbfiBlock . cctxNewBlockInfo
          B.bool (throwError $ ConsensusError "Wrong PoW nonce")
                 (return $ BlockchainBlockInfo bId h b)
                 (checkProofOfWork h b declaredNonce)

        selectTip newBlock = do
          choice <- asks cctxTips
          return $ maximumBy (\b0 b1 -> compare (bbiHeight b0) (bbiHeight b1))
                             (newBlock:choice)

generateConsensus :: Blockchain b => NewBlockFullInfo -> b (Either ConsensusError ConsensusData)
generateConsensus nbfi = do
  ctx <- extractConsensusContext nbfi
  return $ runConsensus consensusGenerator ctx
  where consensusGenerator :: ConsensusComputation ConsensusData
        consensusGenerator = do
          h <- asks cctxBlockHeight
          b <- asks $ nbfiBlock . cctxNewBlockInfo
          return $ ConsensusData { powNonce = proofOfWork h b }


-- this PoW algorithm is from
-- https://github.com/adjoint-io/nanochain/blob/master/src/Nanochain.hs
proofOfWork :: Int -> Block -> Int
proofOfWork height block = proofOfWorkWithBaseNonce height block 0

checkProofOfWork :: Int -> Block -> Int -> Bool
checkProofOfWork height block nonce = proofOfWorkWithBaseNonce height block nonce == nonce

proofOfWorkWithBaseNonce :: Int -> Block -> Int -> Int
proofOfWorkWithBaseNonce height block nonce = calcNonce nonce
  where dbits  = round $ logBase (2 :: Float) $ fromIntegral height 
        prefix = T.pack $ replicate dbits '0'

        calcNonce n | prefix' == prefix = n
                    | otherwise         = calcNonce $ n + 1
          where hash'   = Id.id $ Id.ByBinary block { blockConsensus = ConsensusData {powNonce = n} }
                prefix' = T.take dbits . TE.decodeUtf8 . BS64.encode $ convert hash'
