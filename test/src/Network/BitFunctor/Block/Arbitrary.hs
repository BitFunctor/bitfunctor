module Network.BitFunctor.Block.Arbitrary where

import Network.BitFunctor.Account
import Network.BitFunctor.Block

import Network.BitFunctor.Account.Arbitrary ()
import Network.BitFunctor.Transaction.Arbitrary ()

import Data.Maybe (fromJust)

import Test.QuickCheck
import Test.QuickCheck.Instances()


instance Arbitrary Block where
  arbitrary = do
    generatorAcc <- arbitrary

    p <- arbitrary
    t <- arbitrary
    txsn <- arbitrary
    txs <- vectorOf txsn arbitrary
    tgt <- arbitrary
    let gen = toAccountId generatorAcc

    let block = Block p t txs tgt gen undefined
    return . fromJust $ sign generatorAcc block
