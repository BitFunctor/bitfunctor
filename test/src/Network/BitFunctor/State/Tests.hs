module Network.BitFunctor.State.Test where

import Network.BitFunctor.Account
import Network.BitFunctor.State.Pending
import Network.BitFunctor.Transaction
import Network.BitFunctor.Transaction.Arbitrary()

import Data.Maybe (fromJust)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit (Assertion, (@?=))


tests :: TestTree
tests = testGroup "Network.BitFunctor.State.Tests"
  [ 
  ]



prop_mp_taketx_removes_tx :: PendingTxs mp => mp -> TransactionHash -> Bool
prop_mp_taketx_removes_tx = undefined

-- (getTxs . addTx mp tx) `elem` tx
-- sort $ ((getTxs . addTx mp tx) \\ tx) == sort $ getTxs mp
-- (getTx (hash tx) . addTx mp tx) == tx
prop_mp_addtx_adds_tx :: PendingTxs mp => mp -> Transaction -> Bool
prop_mp_addtx_adds_tx mp tx = undefined

