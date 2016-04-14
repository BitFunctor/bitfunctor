module Network.BitFunctor.Account.Arbitrary where

import Network.BitFunctor.Account
import Network.BitFunctor.Crypto.Arbitrary()

import Test.QuickCheck
import Test.QuickCheck.Instances()


instance Arbitrary Account where
  arbitrary = do
    sk <- arbitrary
    return $ mkAccount sk

instance Arbitrary AccountId where
  arbitrary = do
    acc <- arbitrary
    return $ toAccountId acc
