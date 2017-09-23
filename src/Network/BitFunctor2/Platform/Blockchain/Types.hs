{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.BitFunctor2.Platform.Blockchain.Types where

import Data.UUID

import Network.BitFunctor.Crypto.Hash
import Network.BitFunctor.Crypto.Signing
import Network.BitFunctor2.Platform.Blockchain.Consensus.Types
import Network.BitFunctor.Identifiable

import Data.Binary (Binary(..), encode)
import GHC.Generics (Generic)
import Data.Text (Text)

import Control.Monad.Except

import Network.BitFunctor.Crypto.Hash (hash)
import Data.Binary (Binary (..), encode)
import Data.ByteString.Lazy (toStrict)

data Block = Block
  { blockMeta         :: BlockMeta
  , blockConsensus    :: ConsensusData
  , blockTransactions :: [Hash Id Transaction]
  } deriving (Eq, Show, Generic)

instance Binary Block

instance Identifiable Block where
  id = hash . toStrict . encode


data BlockMeta = BlockMeta
  { 
  -- blockMetaType   :: UUID
  -- , 
  blockMetaPrevId :: Maybe (Hash Id Block)
  } deriving (Eq, Show, Generic)

instance Binary BlockMeta


data Transaction = Transaction
  { transactionMeta    :: TransactionMeta
  , transactionObjects :: [Object]
  } deriving (Eq, Show, Generic)

instance Binary Transaction


data TransactionMeta = TransactionMeta {
} deriving (Eq, Show, Generic)

instance Binary TransactionMeta


data Object = UnknownObjectType ObjectMeta
  deriving (Eq, Show, Generic)

instance Binary Object


data ObjectMeta = ObjectMeta
  { objectMetaId   :: UUID
  , objectMetaType :: UUID
  , objectMetaSignature :: Signature
  } deriving (Eq, Show, Generic)

instance Binary ObjectMeta



data NewBlockFullInfo = NewBlockFullInfo {
  nbfiId    :: Hash Id Block,
  nbfiBlock :: Block,
  nbfiTxs   :: [Transaction]
} deriving (Eq, Show)

data BlockchainBlockInfo = BlockchainBlockInfo {
  bbiId     :: Hash Id Block,
  bbiHeight :: Int,
  bbiBlock  :: Block
} deriving (Eq, Show)


class (Monad m, MonadError BlockchainError m) => Blockchain m where
  newBlock :: NewBlockFullInfo -> m ()
  height   :: Hash Id Block -> m (Maybe Int)
  block    :: Hash Id Block -> m (Maybe Block)
  tips     :: m [Hash Id Block]
  -- mainTip  :: m (Hash Id Block)


data BlockchainConfig = BlockchainConfig {
  --prevBlockSelectionPolicy :: Blockchain b => b (Maybe (Hash Id Block))
} deriving (Eq, Show)

data BlockchainError = NewBlockError Text
                     | ConsensusGenerationError Text
                     | Other Text
  deriving (Eq, Show)

