module Network.BitFunctor.Block.Tests where

import Network.BitFunctor.Account
import Network.BitFunctor.Block
import Network.BitFunctor.Block.Arbitrary ()

import Data.Maybe (fromJust)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit (Assertion, (@?=))


tests :: TestTree
tests = testGroup "Network.BitFunctor.Block.Tests"
  [ testProperty "verify(sign(account, block)) == True" prop_verifysign
  ]


prop_verifysign :: Account -> Block -> Bool
prop_verifysign acc b = (verify $ fromJust $ sign acc b') == True
                       where b' = b { generator = toAccountId acc }
