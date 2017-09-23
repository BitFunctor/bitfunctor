{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.BitFunctor2.Platform.Storage.Backend.Acid where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Acid
import Data.SafeCopy
import Data.Hashable

import qualified Network.BitFunctor.Identifiable as Id
import Network.BitFunctor.Crypto.Hash (Hash, Id)
import Network.BitFunctor2.Platform.Blockchain.Types (Block)

import Network.BitFunctor2.Platform.Storage.Types
import Network.BitFunctor2.Platform.Storage.Backend.Acid.SafeCopy ()



data AcidStorage = AcidStorage {
  acidAllBlocks :: HashMap (Hash Id Block) Block
} deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''AcidStorage)


newtype AcidBackend a = AcidBackend {
  unAcidBackend :: StateT (AcidState AcidStorage) (ReaderT BlockchainStorageConfig (ExceptT BlockchainStorageError IO)) a
} deriving ( Monad
           , Functor
           , Applicative
           , MonadReader BlockchainStorageConfig
           , MonadState (AcidState AcidStorage)
           , MonadIO
           , MonadError BlockchainStorageError
           )

acidPutBlock :: Block -> Update AcidStorage ()
acidPutBlock b = modify (\(AcidStorage blocks) ->
                           AcidStorage (HM.insertWith (\new old -> new)
                                       (Id.id b)
                                       b blocks))

acidGetAllBlocks :: Query AcidStorage [Block]
acidGetAllBlocks = do
  blocks <- asks acidAllBlocks
  return $ HM.elems blocks

acidGetBlock :: Hash Id Block -> Query AcidStorage (Maybe Block)
acidGetBlock bid = HM.lookup bid . acidAllBlocks <$> ask

$(makeAcidic ''AcidStorage ['acidPutBlock, 'acidGetAllBlocks, 'acidGetBlock])

-- examples: https://github.com/acid-state/acid-state/blob/master/examples/HelloDatabase.hs

instance BlockchainStorage AcidBackend where
  putBlock b = do
    acidDb <- get
    result <- liftIO $ update acidDb $ AcidPutBlock b
    return ()
  getBlock bid = do
    acidDb <- get
    block <- liftIO $ query acidDb $ AcidGetBlock bid
    return block
  allBlocks = do
    acidDb <- get
    allBlocks <- liftIO $ query acidDb $ AcidGetAllBlocks
    return allBlocks
