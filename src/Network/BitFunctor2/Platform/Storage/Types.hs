{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.BitFunctor2.Platform.Storage.Types where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import qualified Network.BitFunctor.Identifiable as Id
import Network.BitFunctor.Crypto.Hash (Hash, Id)
import Network.BitFunctor2.Platform.Blockchain.Types (Block)



class BlockchainStorage s where
  putBlock :: Block -> s ()
  getBlock :: Hash Id Block -> s (Maybe Block)
  allBlocks :: s [Block]

class ObjectStorage s where



data BlockchainStorageConfig = BlockchainStorageConfig {
} deriving (Eq, Show)

data BlockchainStorageError = BlockchainStorageError
  deriving (Eq, Show)
