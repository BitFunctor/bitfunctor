module Network.BitFunctor.Tests where

import Network.BitFunctor.Account (AccountId)
import Network.BitFunctor.Block (Block)
import Network.BitFunctor.Crypto.Types (PublicKey, Signature)
import Network.BitFunctor.Transaction (Transaction)
import Network.BitFunctor.Transaction.Arbitrary()

import Data.Binary (Binary, encode, decode)

import Test.QuickCheck

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import Test.HUnit (Assertion, (@?=))


tests :: TestTree
tests = testGroup "Network.BitFunctor.Tests"
  [ testCase "true" $
      True @?= True
  , testProperty "binary_decode(binary_encode(pubkey)) == pubkey" (prop_binary_encdec_inv :: PublicKey -> Bool)
  , testProperty "binary_decode(binary_encode(sig)) == sig" (prop_binary_encdec_inv :: Signature -> Bool)
  , testProperty "binary_decode(binary_encode(accountid)) == accountid" (prop_binary_encdec_inv :: AccountId -> Bool)
  , testProperty "binary_decode(binary_encode(tx)) == tx" (prop_binary_encdec_inv :: Transaction -> Bool)
  , testProperty "binary_decode(binary_encode(block)) == block" (prop_binary_encdec_inv :: Block -> Bool)
  ]

prop_binary_encdec_inv :: (Binary a, Eq a) => a -> Bool
prop_binary_encdec_inv obj = (decode . encode) obj == obj


instance Arbitrary Block where
  arbitrary = undefined
