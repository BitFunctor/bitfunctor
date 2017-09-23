{-# LANGUAGE DeriveGeneric #-}

module Network.BitFunctor2.Platform.Blockchain.Consensus.Types where

import Data.Binary
import GHC.Generics (Generic)


data ConsensusData = ConsensusData {
  powNonce :: Int
} deriving (Eq, Show, Generic)

instance Binary ConsensusData
