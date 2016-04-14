module Network.BitFunctor.Account.Tests where

import Network.BitFunctor.Account
import Network.BitFunctor.Account.Arbitrary()

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit (Assertion, (@?=))


tests :: TestTree
tests = testGroup "Network.BitFunctor.Account.Tests"
  [ testProperty "from_accountid(to_accountid(account)).pubkey == account.pubkey" prop_accountid_inv_over_pk
  ]

prop_accountid_inv_over_pk :: Account -> Bool
prop_accountid_inv_over_pk acc = pubKey (fromAccountId $ toAccountId acc) == pubKey acc

