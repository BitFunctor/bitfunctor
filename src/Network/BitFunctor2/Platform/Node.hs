{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.BitFunctor2.Platform.Node where

import Control.Monad.Reader
import Control.Monad.Except

import Network.BitFunctor2.Platform.Blockchain

data NodeConfig = NodeConfig {
} deriving (Eq, Show)

data NodeError = NodeError
  deriving (Eq, Show)

newtype Node a = Node {
  unNode :: ReaderT NodeConfig (ExceptT NodeError IO) a
} deriving ( Monad
           , Functor
           , Applicative
           , MonadReader NodeConfig
           , MonadIO
           , MonadError NodeError
           )


runNode :: Node a -> NodeConfig -> IO (Either NodeError a)
runNode n c = runExceptT $ runReaderT (unNode n) c

runNodeApp = runNode app c
  where app :: Node ()
        app = undefined
        c = NodeConfig {}
