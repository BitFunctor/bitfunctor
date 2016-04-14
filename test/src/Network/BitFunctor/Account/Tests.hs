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
  , testProperty "from_accountid(to_accountid(account)).seckey == Nothing" prop_accountid_erases_sk
  , testProperty "to_accountid(from_accountid(accountid)) == accountid" prop_accountid_inv_over_accountid
  ]

prop_accountid_inv_over_pk :: Account -> Bool
prop_accountid_inv_over_pk acc = pubKey (fromAccountId $ toAccountId acc) == pubKey acc

prop_accountid_erases_sk :: Account -> Bool
prop_accountid_erases_sk acc = secKey (fromAccountId $ toAccountId acc) == Nothing

prop_accountid_inv_over_accountid :: AccountId -> Bool
prop_accountid_inv_over_accountid accId = (toAccountId . fromAccountId) accId == accId
