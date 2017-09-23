{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.BitFunctor2.Platform.Blockchain where

import Network.BitFunctor2.Platform.Blockchain.Types
import Network.BitFunctor.Crypto.Hash

import Control.Monad.Reader
import Control.Monad.Except

import Network.BitFunctor2.Platform.Blockchain.Consensus



newtype BlockchainApp a = BlockchainApp {
  unBlockchain :: ReaderT BlockchainConfig (ExceptT BlockchainError IO) a
} deriving ( Monad
           , Functor
           , Applicative
           , MonadReader BlockchainConfig
           , MonadIO
           , MonadError BlockchainError
           )

instance Blockchain BlockchainApp where
  newBlock = handleNewBlock
  height bid = return $ Just 3 -- TODO
  tips = return []  -- TODO


runBlockchain :: BlockchainApp a -> BlockchainConfig -> IO (Either BlockchainError a)
runBlockchain b c = runExceptT $ runReaderT (unBlockchain b) c


null :: Blockchain m => m ()
null = return ()

handleNewBlock :: Blockchain m => NewBlockFullInfo -> m ()
handleNewBlock nbfi = do
  let block = nbfiBlock nbfi

  structuralValidity nbfi

  -- atomic (read-wise)
  transactionalValidity nbfi
  newTip <- checkConsensus nbfi >>= either consensusErr return

  apply nbfi newTip
  -- atomic end

  return ()
    where consensusErr e = throwError $ NewBlockError "Consensus error"

structuralValidity :: Blockchain m => NewBlockFullInfo -> m ()
structuralValidity nbfi = return ()

transactionalValidity :: Blockchain m => NewBlockFullInfo -> m ()
transactionalValidity nbfi = return ()

apply :: Blockchain m => NewBlockFullInfo -> BlockchainBlockInfo -> m ()
apply nbfi tip = do
  let block = nbfiBlock nbfi
  let txs = nbfiTxs nbfi
  -- atomic (write-wise)
  applyTxs txs
  applyBlock block tip
  -- atomic end

applyBlock :: Blockchain m => Block -> BlockchainBlockInfo -> m ()
applyBlock blockToAdd newTipToSet = return () -- TODO

applyTxs :: (Blockchain m, Traversable t) => t Transaction -> m ()
applyTxs txsToAdd = return () -- TODO
