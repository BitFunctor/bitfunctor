{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.BitFunctor2.Platform.Storage where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import qualified Network.BitFunctor.Identifiable as Id
import Network.BitFunctor.Crypto.Hash (Hash, Id)
import Network.BitFunctor2.Platform.Blockchain.Types (Block)

import Network.BitFunctor2.Platform.Storage.Types



data BlockchainStorageState = BlockchainStorageState {
} deriving (Eq, Show)

newtype BlockchainStorageApp a = BlockchainStorageApp {
  unBlockchainStorageApp :: StateT BlockchainStorageState (ReaderT BlockchainStorageConfig (ExceptT BlockchainStorageError IO)) a
} deriving ( Monad
           , Functor
           , Applicative
           , MonadReader BlockchainStorageConfig
           , MonadState BlockchainStorageState
           , MonadIO
           , MonadError BlockchainStorageError
           )

instance BlockchainStorage BlockchainStorageApp where

