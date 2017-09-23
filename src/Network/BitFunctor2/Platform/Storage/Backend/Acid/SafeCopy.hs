{-# LANGUAGE TemplateHaskell #-}

module Network.BitFunctor2.Platform.Storage.Backend.Acid.SafeCopy where

import Data.SafeCopy

import qualified Network.BitFunctor.Identifiable as Id
import Network.BitFunctor2.Platform.Blockchain.Types ( Block
                                                     , BlockMeta
                                                     , Transaction
                                                     , TransactionMeta
                                                     , Object
                                                     , ObjectMeta
                                                     )
import Network.BitFunctor2.Platform.Blockchain.Consensus.Types (ConsensusData)
import Data.UUID (UUID (..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable



$(deriveSafeCopy 0 'base ''Id.ByBinary)
$(deriveSafeCopy 0 'base ''Block)
$(deriveSafeCopy 0 'base ''BlockMeta)
$(deriveSafeCopy 0 'base ''Transaction)
$(deriveSafeCopy 0 'base ''TransactionMeta)
$(deriveSafeCopy 0 'base ''Object)
$(deriveSafeCopy 0 'base ''ObjectMeta)
$(deriveSafeCopy 0 'base ''ConsensusData)
$(deriveSafeCopy 0 'base ''UUID)

instance (SafeCopy a, Eq a, Hashable a, SafeCopy b) => SafeCopy (HashMap a b) where
  getCopy = contain $ fmap HM.fromList safeGet
  putCopy = contain . safePut . HM.toList
