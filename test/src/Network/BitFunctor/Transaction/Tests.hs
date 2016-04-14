module Network.BitFunctor.Transaction.Tests where

import Network.BitFunctor.Account
import Network.BitFunctor.Transaction
import Network.BitFunctor.Transaction.Arbitrary()

import Data.Maybe (fromJust)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit (Assertion, (@?=))


tests :: TestTree
tests = testGroup "Network.BitFunctor.Transaction.Tests"
  [ testProperty "verify(sign(account, tx)) == True" prop_verifysign
  ]


prop_verifysign :: Account -> Transaction -> Bool
prop_verifysign acc tx = (verify $ fromJust $ sign acc tx') == True
                       where tx' = tx { input = txi { sender = (toAccountId acc) } }
                             txi = input tx
